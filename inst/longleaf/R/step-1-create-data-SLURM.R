#!/bin/Rscript

# Creates a single SLURM file to generate the models 

# Load libraries 
library(argparse)
library(rjson)

# Read in arguments from CLI
parser <- ArgumentParser(description = "Step 1: Create simulation objects")
parser$add_argument('--models_json_file', required = TRUE, help = "JSON file containing model information")
parser$add_argument('--gen_json_file', required = TRUE, help = "JSON file containing generation parameters")
parser$add_argument('--sim_dir', required = TRUE, help = "Location simulations")
parser$add_argument('--code_dir', required = TRUE, help = "Location of code to run simulations")

# Parse arguments 
args <- parser$parse_args() 

# Fix parameters
gen_json <- fromJSON(file = args$gen_json_file)
num_individuals <- gen_json$num_individuals  
num_mc_samples  <- gen_json$num_mc_samples  

# Define required locations for simulations
slurm_out       <- paste0(args$sim_dir, "/slurm/step-1")
models_dir      <- paste0(args$sim_dir, "/models")
logs_dir        <- paste0(args$sim_dir, "/logs/step-1")

# Overall string header 
SLURM_header <- "#!/bin/bash
#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem=20g
#SBATCH -n 4
#SBATCH -t 3-00:00:00
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
code_to_run <- paste(args$code_dir, "step-1-create-data.R", sep = "/")
run_cmd <- paste("
# Command to run 
Rscript", code_to_run, sep = " ")


# Loop through models here!
model_json_list         <- fromJSON(file = args$models_json_file)
for (mod_i in 1:length(model_json_list$models)) {

    # Model name
    mod_i_name <- args$models_json_file$models[mod_i]

    # SLURM output file locations
    log.o <- paste0("#SBATCH --output=", logs_dir, "/slurm-step-1-", mod_i_name, ".stdout")
    log.e <- paste0("#SBATCH --error=", logs_dir, "/slurm-step-1-", mod_i_name, ".stderr")

    # Update string 
    cmd_string <- run_cmd 
    cmd_string <- paste(cmd_string, "--skeleton_name", mod_i_name, sep = " ")
    cmd_string <- paste(cmd_string, "--skeleton_seed", args$models_json_file$seeds[mod_i], sep = " ")
    cmd_string <- paste(cmd_string, "--models_out_loc", models_dir, sep = " ")
    cmd_string <- paste(cmd_string, "--num_indv_models", num_individuals, sep = " ")
    cmd_string <- paste(cmd_string, "--num_mc_samples", num_mc_samples, sep = " ")

    # Combine all parts together 
    SLURM_out <- paste(c(SLURM_header, log.o, log.e, SLURM_conda, cmd_string, SLURM_end), sep = "\n\n")

    # Create file 
    if (!file.exists(slurm_out)) {
        dir.create(slurm_out, recursive = TRUE)
    }
    file_conn <- file(paste0(slurm_out, "/SLURM_create_data_", mod_i_name, ".slurm"))
    writeLines(SLURM_out, file_conn)
    close(file_conn)

}
