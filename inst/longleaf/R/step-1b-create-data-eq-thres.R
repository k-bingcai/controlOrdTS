# Set seed
set.seed(100)

# Load libraries 
library(argparse)
library(rjson)
library(controlOrdTS)

# Read in arguments from CLI
parser <- ArgumentParser(description = "Step 1: Create simulation objects")
parser$add_argument('--skeleton_name', required = TRUE, help = "Skeleton graph of model to use")
parser$add_argument('--models_out_loc', required = TRUE, help = "Location for models to be saved to")
parser$add_argument('--num_mc_samples', required = TRUE, help = "Number of Monte Carlo sample replicates")

# Parse arguments 
args <- parser$parse_args()

# Number of mc replicates 
num_mc_samples <- as.integer(args$num_mc_samples)

# Maximum number of timepoints 
max_timepts <- 1000

# Convenience function to create folder if not exists 
create_dir_not_exist <- function(dir_name) {
    if (!file.exists(dir_name)) {
        dir.create(dir_name, recursive = TRUE)
    }
}


# Convenience function to save file if it does not already exist 
save_RDS_if_not_exist <- function(file_name, obj_to_save) {
    if (!file.exists(file_name)) {
        saveRDS(obj_to_save, file = file_name)
    } else {
        stop("File to save already exists!")
    }
}

# Add print statements for logging
cat(paste0("[INFO] Starting model and data generation."))
cat("\n")

# `i' is for different skeletons (e.g. bringmann_dataset_1)

# Add print statements for logging
mod_i_orig_name <- args$skeleton_name
mod_i_eqthres_name <- paste(mod_i_orig_name, "_eq_thres", sep = "")
cat(paste0("[INFO] Begin generation for model: ", mod_i_eqthres_name, "\n"))

# Search for model folder 
mod_i_orig_folder <- paste0(args$models_out_loc, "/", mod_i_orig_name)
if (!file.exists(mod_i_orig_folder)) {
    stop(paste("[ERROR] Original", args$skeleton_name, "folder is missing."))
}

# Create output folders 
mod_i_save_folder <- paste0(args$models_out_loc, "/", mod_i_eqthres_name)
create_dir_not_exist(mod_i_save_folder)

# Fixed ordinal thresholds 
fixed_ord_thres <- qnorm(seq(0.1,0.9,length.out = 6))

# Loop through existing model folders
existing_mods <- list.files(mod_i_orig_folder, pattern = "model_")
for (mod_ij in existing_mods) {

    # Add print statements for logging
    cat(paste0("[INFO] Generating for model: ", mod_i_eqthres_name, " ; mc iteration = ", mod_ij))

    # Read original simobj 
    gen_ij_simobj <- readRDS(paste(mod_i_orig_folder, "/",
                                   mod_ij, "/",
                                   simobj, "/",
                                   "simobj_",
                                   mod_ij, 
                                   ".RDS"), sep = "")

    # Replace thresholds with fixed values 
    num_vars <- ncol(gen_ij_simobj$saved_params$raw.thres)
    new_thres <- as.data.frame(t(matrix(rep(fixed_ord_thres, num_vars),
                                        nrow = num_vars, byrow = TRUE)))
    gen_ij_simobj$saved_params$raw.thres <- new_thres

    # Generate time series
    gen_ij_ts      <- controlOrdTS::generate_TS_from_simobj(gen_ij_simobj, 
                                                            num_mc_samples = num_mc_samples, 
                                                            max_timepts = max_timepts)

    # Save locations
    mod_ij_main_saveloc     <- paste0(mod_i_save_folder, "/model_", mod_ij)
    mod_ij_simobj_saveloc   <- paste0(mod_ij_main_saveloc, "/simobj")
    mod_ij_data_saveloc     <- paste0(mod_ij_main_saveloc, "/data")
    create_dir_not_exist(mod_ij_main_saveloc)
    create_dir_not_exist(mod_ij_simobj_saveloc)
    create_dir_not_exist(mod_ij_data_saveloc)

    # Save the model 
    mod_ij_simobj_savename  <- paste0(mod_ij_simobj_saveloc,
                                      "/simobj_model_", mod_ij, ".RDS") 
    save_RDS_if_not_exist(mod_ij_simobj_savename, gen_ij_model)

    # Save the generated data 
    for (ts_k in names(gen_ij_ts)) {
        mod_ijk_TS_savename <- paste0(mod_ij_data_saveloc,
                                        "/TS_list_model_", mod_ij, "_", ts_k, ".RDS")
        save_RDS_if_not_exist(mod_ijk_TS_savename, gen_ij_ts[[ts_k]])
    }


}

# Add print statements for logging
cat(paste0("[INFO] Completed generation for model: ", mod_i_name))
cat("\n")


# Add print statements for logging
cat(paste0("[INFO] Completed model and data generation."))
cat("\n")

