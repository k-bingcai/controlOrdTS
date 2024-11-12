#!/bin/Rscript

# Creates SLURM file for each combination of (skeleton, model_num, ord_num)

# Load libraries 
library(argparse)
library(rjson)

# Read in arguments from CLI
parser <- ArgumentParser(description = "Step 2: Create SLURM files fitting VARs in lavaan")
parser$add_argument('--models_json_file', required = TRUE, help = "JSON file containing model information")
parser$add_argument('--gen_json_file', required = TRUE, help = "JSON file containing generation parameters")
parser$add_argument('--code_dir', required = TRUE, help = "Location of code directory")
parser$add_argument('--sim_dir', required = TRUE, help = "Folder containing the simulation setup from Step 1.")
parser$add_argument('--ord_as_cont', action = 'store_true', help = "Indicator if we should treat ordinal data as continuous") # Default is FALSE

# Parse arguments 
args <- parser$parse_args() 

# Define required locations for simulations
slurm_out       <- paste0(args$sim_dir, "/slurm/step-2")
models_dir      <- paste0(args$sim_dir, "/models")
logs_dir        <- paste0(args$sim_dir, "/logs/step-2")

# Overall string header 
SLURM_header <- "#!/bin/bash
#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem=20g
#SBATCH -n 5
#SBATCH -t 30:00:00
"


# Specify conda environment 
SLURM_conda <- "
# Activate conda environment
source ~/.bashrc
conda activate controlOrdTS
"

# Overall string end
SLURM_end <- "
# Deactivate conda environment 
conda deactivate
"

# General command to use 
code_to_run <- paste(args$code_dir, "step-2-fit-models.R", sep = "/")
run_cmd <- paste("
# Command to run 
Rscript", code_to_run, sep = " ")


# Loop through models here!
gen_json_list           <- fromJSON(file = args$gen_json_file)
model_json_list         <- fromJSON(file = args$models_json_file)


# Conditions to cross
conditions_list <- list(model_num = seq(1,gen_json_list$num_individuals),  
                        skeleton  = model_json_list$models,
                        ord_num   = gen_json_list$num_ord)
conditions_df <- expand.grid(conditions_list)


# Loop through rows of conditions 
for (cond_i in 1:nrow(conditions_df)) {

    # Extract names 
    this_row                <- lapply(c(conditions_df[cond_i,]), as.character)
    this_row_model_num      <- paste0("model_", this_row$model_num)
    this_row_skeleton       <- paste0(this_row$skeleton)
    this_row_ord_num        <- paste0("ord", this_row$ord_num)
    this_row_name           <- paste(this_row_model_num, this_row_skeleton, this_row_ord_num, sep = "_")

    # Name extension for ordascont
    if (args$ord_as_cont) {
        this_row_name <- paste(this_row_name, "ordascont", sep = "_")        
    }


    # SLURM output file locations
    log.o <- paste0("#SBATCH --output=", logs_dir, "/slurm-step-2-", this_row_name, ".stdout")
    log.e <- paste0("#SBATCH --error=", logs_dir, "/slurm-step-2-", this_row_name, ".stderr")

    # Update string 
    cmd_string <- run_cmd 
    cmd_string <- paste(cmd_string, "--sim_folder", args$sim_dir, sep = " ")
    cmd_string <- paste(cmd_string, "--skeleton", this_row$skeleton, sep = " ")
    cmd_string <- paste(cmd_string, "--model_num", this_row$model_num, sep = " ")
    cmd_string <- paste(cmd_string, "--ord_num", this_row$ord_num, sep = " ")
    if (args$ord_as_cont) {
        cmd_string <- paste(cmd_string, "--ord_as_cont", sep = " ")
    }

    # Combine all parts together 
    SLURM_out <- paste(c(SLURM_header, log.o, log.e, SLURM_conda, cmd_string, SLURM_end), sep = "\n\n")

    # Create file 
    if (!file.exists(slurm_out)) {
        dir.create(slurm_out, recursive = TRUE)
    }
    file_conn <- file(paste0(slurm_out, "/SLURM_fit_models_", this_row_name, ".slurm"))
    writeLines(SLURM_out, file_conn)
    close(file_conn)

}


