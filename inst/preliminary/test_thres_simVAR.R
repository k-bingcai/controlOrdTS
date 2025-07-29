setwd("~/Desktop/UNC-CH/Projects/controlOrdTS-main/controlOrdTS")
library(devtools)
load_all()


# Change this
use_skew <- TRUE

# Thresholds to use
thres_use <- 1:6



nvars <- 6
Psi <- random_Psi(nvars)
Phi <- bringmann_2017_dataset1()
sVAR_params <- standardize_VAR(Phi, Psi)
newVAR <- simulateVAR$new(Phi = sVAR_params$Phi,
                          Psi = sVAR_params$Psi,
                          skewed_thres = use_skew)
newVAR$initialize_models()

actual_thres <- newVAR$saved_params$raw.thres
xs      <- seq(-3, 3, length.out = 1000)
density <- dnorm(xs)
par(mfcol = c(nvars,4))
for (j in 1:nvars) {

  plot(xs, density, type = "l")
  for (x in 1:(7-1)) {
    abline(v = actual_thres[thres_use,][[paste0("V",j)]][x],
           col = 'red', lty = 2)
  }

}


# Compute skew
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

# Generate data
gen_ts <- newVAR$generate_ts_from_model()

# Actual quantiles used
newVAR$saved_params$rand_unsummed_quantiles

# Try different ordinal outputs
for (numorduse in c("7", "5", "3")) {
  for (v_i in 1:nvars) {

    ord_data <- gen_ts$ord.ts[[numorduse]][[paste0("V", v_i)]]
    probs    <- table(ord_data) / length(ord_data)
    skew_v_i <- skew_disc(probs)
    barplot(probs, main = paste("Skew:", round(skew_v_i, 3)))


  }
}



# Rhumtella's
# Normal - 3 cat - skew = -1.41
# Normal - 5 cat - skew = -0.90
# Normal - 7 cat - skew = -0.78

# Also check the actual category distribution (in case there are bugs)
