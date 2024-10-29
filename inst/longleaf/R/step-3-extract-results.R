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

# Parse arguments 
args <- parser$parse_args()

# Read in the data corresponding to the arguments
model_string 	  <- paste("model", args$model_num, sep = "_")
RDS_filename_stem <- paste0("TS_list_", model_string, "_ord", args$ord_num)
RDS_filename_full <- paste0(RDS_filename_stem, "_fitted.RDS")
model_loc <- paste(args$sim_folder,
		           "models",
                   args$skeleton, 
                   model_string,
		           sep = "/")
fitted_file <- paste(model_loc,
                     "fitted",
                     RDS_filename_full, sep = "/")

# Read in simobj that corresponds to the fit (for true parameters)
true_mod_name <- paste0("simobj_", model_string, ".RDS")
true_mod_file <- paste(model_loc,
                       "simobj",
                       true_mod_name,
                       sep = "/")

# Read in the models
cat(paste0("[INFO] Reading fitted model from ", model_loc, "\n")) 
fitted_obj  <- readRDS(fitted_file)
true_obj    <- readRDS(true_mod_file)

# Extract true parameters and compute results
tPhi        <- true_obj$saved_params$Phi
tPsi        <- true_obj$saved_params$Psi
out_list    <- controlOrdTS::extract_mc_results(fitted_obj, tPhi, tPsi)

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

# Create folders and save output 
cat(paste0("[INFO] Saving extracted results... \n")) 
save_loc    <- paste(model_loc, "results", sep = "/")
save_file   <- paste0(RDS_filename_stem, "_results.RDS")
create_dir_not_exist(save_loc)
save_RDS_if_not_exist(paste(save_loc, save_file, sep = "/"), out_list)




