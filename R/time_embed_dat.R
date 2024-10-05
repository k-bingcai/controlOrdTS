# Performs time-embedding for the time series data
time_embed_dat <- function(raw_dat, max_lag = 1) {

  # Interior function to create lag-n data
  create_lag_n_dat <- function(dat = raw_dat, lag) {

    # Check if lag is valid
    if (lag > nrow(dat) - 1 || lag < 0) {
      stop("Lag value must be non-zero and must be lower than number of rows")
    } else {
      # Pushes down the matrix down by the lag order
      # and fills up the missing space with NAs
      truncated_dat           <- dat[1:(nrow(dat)-lag),]
      colnames(truncated_dat) <- paste(colnames(truncated_dat), "_lag_", lag, sep = "")
      fill_na_dat             <- as.data.frame(matrix(NA, nrow = lag , ncol = ncol(dat)))
      colnames(fill_na_dat)   <- colnames(truncated_dat)
      lag_n_dat               <- rbind(fill_na_dat, truncated_dat)

      # Copying ordering of variables if any
      for (c_dat in 1:ncol(dat)) {
        if (is.ordered(dat[,c_dat])) {
          lag_n_dat[,c_dat] <- factor(lag_n_dat[,c_dat],
                                      levels = levels(dat[,c_dat]),
                                      ordered = TRUE)
        }
      }

    }

    return(lag_n_dat)
  }

  # Combine all the lagged dataframes
  all_lag_dat <- do.call(cbind,
                         mapply(create_lag_n_dat,
                                lag = seq(0, max_lag),
                                SIMPLIFY = FALSE))

  return(all_lag_dat)
}


