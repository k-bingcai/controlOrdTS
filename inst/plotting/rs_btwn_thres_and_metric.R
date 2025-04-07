# --- Supplementary script to check skewness of distribution --- #

# Clear environment
rm(list=ls())
cat("\014")

# Source functions
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Function for KL divergence (discrete)
KL_div_disc <- function(p,q) {
  stopifnot(sum(p) == 1)
  stopifnot(sum(q) == 1)
  stopifnot(length(p) == length(q))
  return(sum(p * (log(p) - log(q))))
}

# Function to compute skew (discrete)
skew_disc <- function(p) {
  stopifnot(abs(1 - sum(p)) < 1e-7)
  # Compute moments
  # Assume categories from 1 to length(p)
  k           <- length(p)
  m1          <- sum(seq(1,k) * p)
  p_variance  <- sum((seq(1,k) - m1)^2 * p)
  p_sigma     <- sqrt(p_variance)
  m3          <- sum(seq(1,k)^3 * p)
  p_skew      <- (m3 - 3 * m1 * p_sigma^2 - m1^3) / (p_sigma^3)
  return(p_skew)
}



# We first extract the model thresholds
# bringmann_dataset1 only for now
# Set to 100 models
extract_skew <- function(sim_dir, numord) {

  # Check if directories exist
  skeleton <- "bringmann_2017_dataset1"
  skeleton_dir <- paste(sim_dir, "models", skeleton, sep = "/")
  stopifnot(file.exists(skeleton_dir))

  # Loop through models in skeleton_dir
  mod_files <- list.files(skeleton_dir)
  skew_list <- vector("list", length(mod_files))
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

    # Extract thresholds
    thres_i <- simobj_i_Robj$saved_params$raw.thres

    # Function to discretize the thresholds
    if (numord == 7) {
      sub_thres <- function(x) {x}
    } else if (numord == 5) {
      sub_thres <- function(x) {c(x[1], x[2], x[5], x[6])}
    } else if (numord == 3) {
      sub_thres <- function(x) {c(x[1], x[6])}
    } else {
      stop("Invalid numord.")
    }

    # Calculate the proportions for each ordinal category
    thres_sub_i   <- apply(thres_i, 2, sub_thres)
    cumprob_i     <- apply(thres_sub_i, 2, pnorm)
    numvars_i     <- dim(thres_i)[2]
    cumprob_ext_i <- rbind(rep(0,numvars_i),
                           cumprob_i,
                           rep(1,numvars_i))
    catprobs_i    <- apply(cumprob_ext_i, 2, diff, lag = 1)

    # Use skewness instead
    skew_i  <- apply(catprobs_i, 2, skew_disc)
    skew_list[[mod_i]] <- skew_i
  }

  return(do.call(rbind, skew_list))

}


# Specifics
sim_date <- "20250115_111042"
sim_folder <- "/home/bing/Desktop/UNC-CH/projects/controlOrdTS_all/resources/simulations/"

# Extract models
skew7 <- extract_skew(paste0(sim_folder, sim_date), numord = 7)
skew5 <- extract_skew(paste0(sim_folder, sim_date), numord = 5)
skew3 <- extract_skew(paste0(sim_folder, sim_date), numord = 3)

hist(as.vector(skew7), breaks = 20)
hist(as.vector(skew5), breaks = 20)
hist(as.vector(skew3), breaks = 20)

# i.e. there is some "moderate" asymmetry according to rhumtella's paper

