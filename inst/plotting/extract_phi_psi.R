# --- Supplementary script to check range of parameters --- #

# Clear environment
rm(list=ls())
cat("\014")

# Source functions
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Function to extract parameters
extract_phi_psi <- function(sim_dir, skeleton, param_type) {

  # Check if directory exists
  skeleton_dir <- paste(sim_dir, "models", skeleton, sep = "/")
  stopifnot(file.exists(skeleton_dir))

  # Loop through models in skeleton_dir
  mod_files <- list.files(skeleton_dir)
  param_list <- vector("list", length(mod_files))
  for (mod_i in mod_files) {

    # Get filepath
    simobj_i_filename <- paste("simobj_", mod_i, ".RDS", sep= "")
    simobj_i_filepath <- paste(skeleton_dir,
                               mod_i,
                               "simobj",
                               simobj_i_filename,
                               sep = "/")

    # Read the file
    if (file.exists(simobj_i_filepath)) {
      simobj_i_Robj <- readRDS(simobj_i_filepath)
    } else {
      cat(paste0("[WARNING] ", simobj_i_filepath, " is missing\n"))
      next
    }

    # Extract parameter
    param_i <- simobj_i_Robj$saved_params[[param_type]]
    param_list[[mod_i]] <- param_i

  }

  return(param_list)
}

# Function to extract mean and sd of abs values
summarize_params <- function(sim_dir, skeleton, param_type) {

  # Get parameters in list form
  param_out <- extract_phi_psi(sim_dir, skeleton, param_type)
  param_vec <- do.call(rbind, lapply(param_out, function(z) {z[z!=0]}))

  # Print
  cat("\n### Information \n")
  cat(paste0("Skeleton: \t", skeleton, "\n"))
  cat(paste0("Parameter: \t", param_type, "\n"))
  cat(paste0("Mean: \t\t", round(mean(abs(param_vec)), 3), "\n"))
  cat(paste0("SD: \t\t", round(sd(abs(param_vec)), 3), "\n"))

}

# Specifics
sim_date <- "20250115_111042"
sim_folder <- "/home/bing/Desktop/UNC-CH/projects/controlOrdTS_all/resources/simulations/"

# bringmann_2017_dataset1
summarize_params(paste0(sim_folder, sim_date),
                 "bringmann_2017_dataset1",
                 "Phi")
summarize_params(paste0(sim_folder, sim_date),
                 "bringmann_2017_dataset1",
                 "Psi")

# bringmann_2017_dataset2
summarize_params(paste0(sim_folder, sim_date),
                 "bringmann_2017_dataset2",
                 "Phi")
summarize_params(paste0(sim_folder, sim_date),
                 "bringmann_2017_dataset2",
                 "Psi")

# scale_free_seeded
summarize_params(paste0(sim_folder, sim_date),
                 "scale_free_seeded",
                 "Phi")
summarize_params(paste0(sim_folder, sim_date),
                 "scale_free_seeded",
                 "Psi")


# # Extract params
# phi_out <- extract_phi_psi(paste0(sim_folder, sim_date),
#                            "bringmann_2017_dataset1",
#                             "Phi")
# phi_vec_out <- do.call(rbind, lapply(phi_out, function(z) {z[z!=0]}))
# mean(abs(phi_vec_out))
# sd(abs(phi_vec_out))


