# This is different from the other extract results;
# Set seed (this is not necessary but whatever)
set.seed(100)

# Load libraries 
library(argparse)
library(controlOrdTS)

# Read in arguments from CLI
parser <- ArgumentParser(description = "Step 3: Extract results from fitted models")
parser$add_argument('--sim_folder', required = TRUE, help = "Location of simulation folder")
parser$add_argument('--skeleton', required = TRUE, help = "Type of skeleton used to generate the data")
parser$add_argument('--model_num', required = TRUE, help = "Model number to identify which folder to use")
parser$add_argument('--ord_num', required = TRUE, help = "Number of ordinal categories to identify which generated data to use")
parser$add_argument('--ord_as_cont', action = 'store_true', help = "Indicator if we should treat ordinal data as continuous") # Default is FALSE

# Parse arguments 
args <- parser$parse_args()

# Save as diffrent file if ord_as_cont
if (args$ord_as_cont) {
    read_file_leaf <- "_fitted_ordascont.RDS"
} else {
    read_file_leaf <- "_fitted.RDS"
}

# Read in the data corresponding to the arguments
model_string 	  <- paste("model", args$model_num, sep = "_")
RDS_filename_stem <- paste0("TS_list_", model_string, "_ord", args$ord_num)
RDS_filename_full <- paste0(RDS_filename_stem, read_file_leaf)

# Pre model and fitted file 
pre_model_loc <- paste(args$sim_folder,
		               "models",
                       args$skeleton, 
                       model_string,
		               sep = "/")
pre_fitted_file <- paste(pre_model_loc,
                         "fitted",
                        RDS_filename_full, sep = "/")                       

# Post model and fitted file 
post_model_loc <- paste(args$sim_folder,
		                "models",
                        paste(args$skeleton, "_prepost", sep = ""), 
                        model_string,
		                sep = "/")
post_fitted_file <- paste(post_model_loc,
                          "fitted",
                          RDS_filename_full, sep = "/")  


# Read in simobj that corresponds to the fit (for true parameters)
true_mod_name <- paste0("simobj_", model_string, ".RDS")
pre_true_mod_file <- paste(pre_model_loc,
                           "simobj",
                           true_mod_name,
                           sep = "/")
post_true_mod_file <- paste(post_model_loc,
                           "simobj",
                           true_mod_name,
                           sep = "/")

# Read in the models
cat(paste0("[INFO] Reading fitted model from ", pre_model_loc, "\n")) 
pre_fitted_obj  <- readRDS(pre_fitted_file)
pre_true_obj    <- readRDS(pre_true_mod_file)

cat(paste0("[INFO] Reading fitted model from ", post_model_loc, "\n")) 
post_fitted_obj  <- readRDS(post_fitted_file)
post_true_obj    <- readRDS(post_true_mod_file)

# Extract true parameters and compute results
pre_tPhi        <- pre_true_obj$saved_params$Phi
post_tPhi       <- post_true_obj$saved_params$Phi

# Extract results 
out_list <- controlOrdTS::extract_mc_results_prepost(mc_pre_fitted_list = pre_fitted_obj,
                                                     mc_post_fitted_list = post_fitted_obj,
                                                     pre_true_Phi = pre_tPhi,
                                                     post_true_Phi = post_tPhi)


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


# Save as diffrent file if ord_as_cont
if (args$ord_as_cont) {
    save_file_leaf <- "_results_ordascont.RDS"
} else {
    save_file_leaf <- "_results.RDS"
}

# Create folders and save output (SAVE IN POST FOLDER)
cat(paste0("[INFO] Saving extracted results... \n")) 
save_loc    <- paste(post_model_loc, "results", sep = "/")
save_file   <- paste0(RDS_filename_stem, save_file_leaf)
create_dir_not_exist(save_loc)
save_RDS_if_not_exist(paste(save_loc, save_file, sep = "/"), out_list)