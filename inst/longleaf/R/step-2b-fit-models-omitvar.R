# Set seed
set.seed(100)

# Load libraries 
library(argparse)
library(controlOrdTS)

# Read in arguments from CLI
parser <- ArgumentParser(description = "Step 2: Fit models to the generated data")
parser$add_argument('--sim_folder', required = TRUE, help = "Location of simulation folder")
parser$add_argument('--skeleton', required = TRUE, help = "Type of skeleton used to generate the data")
parser$add_argument('--model_num', required = TRUE, help = "Model number to identify which folder to use")
parser$add_argument('--ord_num', required = TRUE, help = "Number of ordinal categories to identify which generated data to use")
parser$add_argument('--ord_as_cont', action = 'store_true', help = "Indicator if we should treat ordinal data as continuous") # Default is FALSE
parser$add_argument('--omit_var_num', required = TRUE, help = "Column index to ignore")

# Parse arguments 
args <- parser$parse_args()

# Read in the data corresponding to the arguments
model_string 	  <- paste("model", args$model_num, sep = "_")
RDS_filename_stem <- paste0("TS_list_", model_string, "_ord", args$ord_num)
RDS_filename_full <- paste0(RDS_filename_stem, ".RDS")
model_loc <- paste(args$sim_folder,
		           "models",
                   args$skeleton, 
                   model_string,
		           sep = "/")
data_file <- paste(model_loc,
                   "data",
                   RDS_filename_full, sep = "/")

# Only specify for this model for now
stopifnot(args$skeleton == "bringmann_2017_dataset2")

# Read in data 
data_list <- readRDS(data_file)

# Omit the variable 
omitted_var             <- paste0("V", args$omit_var_num)
data_list_omit_var_tmp  <- lapply(data_list,
                                  function(x) {dplyr::select(x, subset = -c(all_of(omitted_var)))}
                                  )

# Extract new column names
identicalValue <- function(x,y) if (identical(x,y)) x else FALSE
col_mapping <- Reduce(identicalValue, lapply(data_list_omit_var_tmp, colnames))
if (is.logical(col_mapping)) {
    if (col_mapping == FALSE) {
        stop("Columns are not identical in data list with omitted variables.")
    }
}

# Rename columns to fit into lavaan_VAR function 
data_list_omit_var <- lapply(data_list_omit_var_tmp,
                             function(x) {colnames(x) <- paste0("V", seq(1,ncol(x))); x}
                            )

# Fit the models
cat(paste0("[INFO] Reading data from ", model_loc, "\n")) 
cat(paste0("[INFO] Commencing fit for ", RDS_filename_stem, "\n"))
cat(paste0("[INFO] Omitting variable ", omitted_var, "\n"))
lav_fit_all <- controlOrdTS::run_simulation_multi_ts(data_list_omit_var,
                                                     ord_as_cont = args$ord_as_cont,
                                                     save_fit = FALSE,                  # Disables save of fitted model to save space
                                                     mc.cores = 5)
cat(paste0("[INFO] Finished fit for ", RDS_filename_stem, "\n"))


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
    save_file_leaf <- "_fitted_ordascont.RDS"
} else {
    save_file_leaf <- "_fitted.RDS"
}


# Add original column mapping to the output
output_to_save <- list(fit_output = lav_fit_all,
                       omitted_var = omitted_var,
                       col_mapping = col_mapping)

# Create folders and save output 
save_loc    <- paste(model_loc, "fitted/omit_var", sep = "/")
save_file   <- paste0(RDS_filename_stem, save_file_leaf)
create_dir_not_exist(save_loc)
save_RDS_if_not_exist(paste(save_loc, save_file, sep = "/"), output_to_save)