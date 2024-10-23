#!/bin/Rscript

# Creates SLURM file for each combination of (skeleton, model_num, ord_num)

# Load libraries 
library(argparse)

# Read in arguments from CLI
parser <- ArgumentParser(description = "Step 2: Create SLURM files fitting VARs in lavaan")
parser$add_argument('--code_dir', required = TRUE, help = "Location of code directory")
parser$add_argument('--sim_folder', required = TRUE, help = "Folder containing the simulation setup from Step 1.")

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
#SBATCH -t 05:00:00
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

