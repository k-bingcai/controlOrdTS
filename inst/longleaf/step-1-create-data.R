# Set seed
set.seed(100)

# Load libraries 
library(argparse)
library(rjson)
library(controlOrdTS)

# Read in arguments from CLI
parser <- ArgumentParser(description = "Step 1: Create simulation objects")
parser$add_argument('--models_json_file', required = TRUE, help = "JSON file containing simulation conditions")
parser$add_argument('--models_out_loc', required = TRUE, help = "Location for models to be saved to")
parser$add_argument('--num_indv_models', required = TRUE, help = "Number of individual models to create for each skeleton model")
parser$add_argument('--num_mc_samples', required = TRUE, help = "Number of Monte Carlo sample replicates")

# Parse arguments 
args <- parser$parse_args()

# Read in models to create from JSON
model_json_list         <- fromJSON(file = args$models_json_file)
model_json_list$seeds   <- suppressWarnings(as.integer(model_json_list$seeds))

# Number of individual models to create for each skeleton model
num_random <- as.integer(args$num_indv_models)

# Number of mc replicates 
num_mc_samples <- as.integer(args$num_mc_samples)

# Maximum number of timepoints 
max_timepts <- 500

# Convenience function to create folder if not exists 
create_dir_not_exist <- function(dir_name) {
    if (!file.exists(dir_name)) {
        dir.create(dir_name)
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

# Loop through models
for (mod_i in 1:length(model_json_list$models)) {

    # `i' is for different skeletons (e.g. bringmann_dataset_1)

    # Add print statements for logging
    mod_i_name <- model_json_list$models[mod_i]
    cat(paste0("[INFO] Begin generation for model: ", mod_i_name, "\n"))

    # Create folders 
    mod_i_save_folder <- paste0(args$models_out_loc, "/", mod_i_name)
    create_dir_not_exist(mod_i_save_folder)

    # Create mod_arg_list
    mod_i_arg_list    <- list()
    mod_i_seed      <- model_json_list$seeds[mod_i]
    if (!is.na(mod_i_seed)) {
        mod_i_arg_list$seed <- mod_i_seed
    }

    # Function to call 
    mod_i_func    <- mod_i_name

    # Loop through random j iterations of this model 
    for (mod_ij in 1:num_random) {

        # `j' is for different `people' for a given skeleton model

        # Add print statements for logging
        cat(paste0("[INFO] Generating for model: ", mod_i_name, " ; mc iteration = ", mod_ij))

        # Create model 
        gen_ij_model   <- controlOrdTS::create_sim_obj(phi.func = get(mod_i_func),
                                                       phi.func.args = mod_i_arg_list)
        gen_ij_ts      <- controlOrdTS::generate_TS_from_simobj(gen_ij_model, 
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
    cat(paste0("[INFO] Completed generation for model: ", mod_i))
    cat("\n")

}

# Add print statements for logging
cat(paste0("[INFO] Completed model and data generation."))
cat("\n")
