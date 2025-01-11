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
model_loc         <- paste(args$sim_folder,
                           "models",
                           args$skeleton, 
                           model_string,
                           sep = "/")

# Read in simobj that corresponds to the fit (for true parameters)
true_mod_name <- paste0("simobj_", model_string, ".RDS")
true_mod_file <- paste(model_loc,
                       "simobj",
                       true_mod_name,
                       sep = "/")
true_obj      <- readRDS(true_mod_file)

# Extract true parameters and compute results
tPhi        <- true_obj$saved_params$Phi
tPsi        <- true_obj$saved_params$Psi

# Check that only bringmann_dataset_2 is specified
stopifnot(args$skeleton == "bringmann_2017_dataset2")

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


# Loop through each variable that was omitted
omitted_var_id <- 1:10
for (ov_id in omitted_var_id) {

    # Read in data corresponding to arguments
    RDS_filename_stem <- paste0("TS_list_", model_string, "_ord", args$ord_num,
                                "_omit_V", ov_id)
    RDS_filename_full <- paste0(RDS_filename_stem, read_file_leaf)
    fitted_file <- paste(model_loc,
                         "fitted/omit_var",
                         RDS_filename_full, sep = "/")

    # Read in the models
    cat(paste0("[INFO] Reading fitted file from ", fitted_file, "\n")) 
    fitted_obj  <- readRDS(fitted_file)

    # Extract results 
    out_list    <- controlOrdTS::extract_mc_results_omitvar(fitted_obj, tPhi, tPsi, ov_id)

    # Create folders and save output 
    cat(paste0("[INFO] Saving extracted results for omission of V", ov_id, " ... \n")) 
    save_loc    <- paste(model_loc, "results/omit_var", sep = "/")
    save_file   <- paste0(RDS_filename_stem, save_file_leaf)
    create_dir_not_exist(save_loc)
    save_RDS_if_not_exist(paste(save_loc, save_file, sep = "/"), out_list)

}

cat("[INFO] Done.")