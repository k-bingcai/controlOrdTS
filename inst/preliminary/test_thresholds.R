rm(list=ls())

library(extraDistr)
library(ggplot2)

# Variables
numvars <- 5
numordmax <- 7

# Only allow 7 for now
stopifnot(numordmax == 7)

# Number of observed ordinal categories
numorduse <- 7

# Generate random thresholds
rand_unsummed_quantiles_orig   <- extraDistr::rdirichlet(n = numvars,
                                                    alpha = exp(seq(from = 1,
                                                                    by = 0.3,
                                                                    length.out = 7))) # rep(5, numordmax))

# Randomly reverse each row of a matrix
rand_unsummed_quantiles <- t(apply(rand_unsummed_quantiles_orig, 1, function(row) {
  if (runif(1) < 0.5) {
    rev(row)  # Flip the row
  } else {
    row       # Keep it as is
  }
}))



person_i_q_thres          <- t(apply(rand_unsummed_quantiles, 1, cumsum))
# Drop last column of 1s and store as list
person_i_q_thres <- as.matrix(person_i_q_thres[,1:(ncol(person_i_q_thres)-1)])
person_i_q_thres <- as.list(data.frame(t(person_i_q_thres)))

actual_thres <- as.data.frame(mapply(function(x,y) {qnorm(x)},
                              x = person_i_q_thres,
                              SIMPLIFY = FALSE))

# Select certain thresholds
if (numorduse == 7) {
  thres_use <- 1:6
} else if (numorduse == 5) {
  # thres_use <- c(1,2,5,6)
  thres_use <- c(2,3,4,5) # Drop extreme categories 
} else if (numorduse == 3) {
  # thres_use <- c(1,6)
  thres_use <- c(3,4) # Drop extreme categories
}


# Create plots of thresholds 
par(mfcol = c(numvars,2))

xs      <- seq(-3, 3, length.out = 1000)
density <- dnorm(xs)

for (j in 1:numvars) {
  
  plot(xs, density, type = "l")
  for (x in 1:(numordmax-1)) {
    abline(v = actual_thres[thres_use,][[paste0("X",j)]][x],
           col = 'red', lty = 2)
  }
  
}

# plot(xs, density, type = "l")
# for (x in 1:(numordmax-1)) {
#   abline(v = actual_thres$X1[x],
#          col = 'red', lty = 2)
# }
# 
# 
# plot(xs, density, type = "l")
# for (x in 1:(numordmax-1)) {
#   abline(v = actual_thres$X2[x],
#          col = 'red', lty = 2)
# }
# 
# 
# 
# plot(xs, density, type = "l")
# for (x in 1:(numordmax-1)) {
#   abline(v = actual_thres$X3[x],
#          col = 'red', lty = 2)
# }



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

for (j in 1:numvars) {
  raw_probs_j <- rand_unsummed_quantiles[j,] 
  use_probs_j <- rep(NA, numorduse)
  if (numorduse == 7) {
    use_probs_j <- raw_probs_j
  } else if (numorduse == 5) {
    # use_probs_j[1:2] <- raw_probs_j[1:2]
    # use_probs_j[3] <- sum(raw_probs_j[3:5])
    # use_probs_j[4:5] <- raw_probs_j[6:7]
    use_probs_j[1] <- sum(raw_probs_j[1:2])
    use_probs_j[2:4] <- raw_probs_j[3:5]
    use_probs_j[5] <- sum(raw_probs_j[6:7])
  } else if (numorduse == 3) {
    # use_probs_j[1] <- raw_probs_j[1]
    # use_probs_j[2] <- sum(raw_probs_j[2:6])
    # use_probs_j[3] <- raw_probs_j[7]
    use_probs_j[1] <- sum(raw_probs_j[1:3]) 
    use_probs_j[2] <- sum(raw_probs_j[4])
    use_probs_j[3] <- sum(raw_probs_j[5:7])
  }
  skew_j <- skew_disc(use_probs_j)
  barplot(use_probs_j, main = paste("Skew:", round(skew_j, 3)))
}
# skew1 <- skew_disc(rand_unsummed_quantiles[1,])
# skew2 <- skew_disc(rand_unsummed_quantiles[2,])
# skew3 <- skew_disc(rand_unsummed_quantiles[3,])
# 
# 
# # Create bar plot
# # par(mfrow = c(3,1))
# barplot(rand_unsummed_quantiles[1,], main = paste("Skew:", round(skew1, 3)))
# barplot(rand_unsummed_quantiles[2,], main = paste("Skew:", round(skew2, 3)))
# barplot(rand_unsummed_quantiles[3,], main = paste("Skew:", round(skew3, 3)))



# --- Check raw distribution of categories
# It should match approximately the true distribution
# We can just do this for the 6-variable version 
# We can check pearson correlation vs polychoric for extreme asymmetry vs non-extreme





