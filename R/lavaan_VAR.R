#' Wrapper to fit a VAR in lavaan using the block-toeplitz method
#' 
#' @export
fit_lavaan_VAR <- function(input_ts,
                           force_reorder = FALSE,
                           ord_as_cont = FALSE,
                           estimator = "DEFAULT",
                           save_fit = TRUE) {

    # Lag order (set to 1)
    lavaan_lag_order  <- 1

    # Number of variables 
    num_vars <- ncol(input_ts)

    # Check the validity of the estimator 
    permissible_estimators <- c("ML", "DWLS", "ULS", "DEFAULT")
    if (!(estimator %in% permissible_estimators)) {
        stop("[ERROR] Invalid estimators specified.")
    }

    # Forcefully convert all ordinal variables to continuous 
    if (ord_as_cont) {
        input_ts <- as.data.frame(lapply(input_ts, as.numeric))
    }

    # WARNING: we currently cannot handle the case where there is a mix of ordinal and continuous variables
    if (any(sapply(input_ts, is.ordered))) {
        if (all(sapply(input_ts, is.ordered))) {
        use_ordinal <- TRUE
        } else {
        stop("[ERROR] Not implemented yet.")
        }
    } else {
        use_ordinal <- FALSE
    }

    # Set estimator to DWLS if ordinal variables are used
    if (estimator == "DEFAULT") {
        if (use_ordinal) {
            estimator <- "DWLS"
        } else {
            estimator <- "ML"
        }
    }


    # ---- Some convenience functions ---- #

    # Convenience function to calculate thresholds based on unlagged values
    calc_ord_thres <- function(ordinal_vec) {

        # Check if input vector is factor
        stopifnot(is.ordered(ordinal_vec))

        # Calculate thresholds based on gaussian (0,1) model
        ordinal_counts    <- table(ordinal_vec)
        ordinal_cumprops  <- cumsum(ordinal_counts / sum(ordinal_counts))
        ordinal_thres     <- head(qnorm(ordinal_cumprops), -1)

        # Return value
        return(ordinal_thres)

    }


    # Convenience function to reorder TS
    reorder_ts <- function(ts_to_reorder) {

        # Reorder levels if necessary
        ordered_cols <- which(sapply(ts_to_reorder, is.ordered) == TRUE)

        # Loop through ordered columns
        for (ord_col in ordered_cols) {

            # Check if all levels are present in the data
            tmp_ord_vec <- ts_to_reorder[,ord_col]
            all_levels_present <- all(levels(tmp_ord_vec) %in% unique(tmp_ord_vec))

            # Either throw an error or reorder forcefully
            if (!all_levels_present) {
                if (force_reorder) {
                # Forcefully reorder according to the numeric values
                new_order            <- sort(as.integer(unique(tmp_ord_vec)))
                ts_to_reorder[,ord_col]  <- factor(ts_to_reorder[,ord_col], levels = new_order)
                } else {
                stop("[ERROR] Some levels in a factor variable do not appear in the data.")
                }
            }

        }

        return(ts_to_reorder)
    }

    # ---- End of convenience functions ---- #

    # Compute thresholds
    if (use_ordinal) {
        data_thres <- lapply(reorder_ts(input_ts), calc_ord_thres)
    } else {
        data_thres <- NULL
    }

    # Create time embedded
    rows_to_keep_after_embedding  <- (lavaan_lag_order+1):nrow(input_ts)
    lavaan_ts                     <- reorder_ts(time_embed_dat(input_ts,
                                                               max_lag = lavaan_lag_order)[rows_to_keep_after_embedding,])

    # SANITY CHECK (MISSING DATA NOT SUPPORTED FOR NOW)
    stopifnot(sum(is.na(lavaan_ts)) == 0)

    # Check if categories are consistent across lags
    omit_var_k_thres <- NULL
    for (var_k in 1:num_vars) {
        lag_0_uniq_vals <- unique(lavaan_ts[[paste0("V", var_k, "_lag_0")]])
        lag_1_uniq_vals <- unique(lavaan_ts[[paste0("V", var_k, "_lag_1")]])
        if (all(lag_0_uniq_vals %in% lag_1_uniq_vals) && all(lag_1_uniq_vals %in% lag_0_uniq_vals)) {
        omit_var_k_thres <- c(omit_var_k_thres, FALSE)
        } else {
        omit_var_k_thres <- c(omit_var_k_thres, TRUE)
        }
    }

    # Generate lavaan model
    lavaan_mod <- gen_lavaan_dfm_syntax(num_vars = num_vars,
                                        var_thres = data_thres,
                                        omit_thres = omit_var_k_thres,
                                        ordinal = use_ordinal)

    # Fit the model 
    warning_bool    <- FALSE
    error_bool      <- FALSE
    lavaan_fit      <- tryCatch(
        {
            # Try to fit the lavaan model and catch any warnings / errors
            lavaan_fit_tmp <- lavaan::sem(model = lavaan_mod,
                                          data = reorder_ts(lavaan_ts),
                                          ordered = use_ordinal, 
                                          estimator = estimator)
        }, warning = function(w) {
            warning_bool <<- TRUE
            suppressWarnings({
                # Fit again anyway since lavaan uses the same starting values
                lavaan_fit_tmp <- lavaan::sem(model = lavaan_mod,
                                              data = reorder_ts(lavaan_ts),
                                              ordered = use_ordinal,
                                              estimator = estimator)
            })
            return(lavaan_fit_tmp)
        }, error = function(e) {
            error_bool <<- TRUE
            return(NULL)
        }
    )

    # Additional check to see if data has columns with only 1 category due to truncated time series
    one_cat_only_warning <- any(unlist(lapply(reorder_ts(lavaan_ts), function(y) length(unique(y)) == 1)))

    # Extract VAR params
    if (!is.null(lavaan_fit)) {
      est_Phi     <- extract_lavaan_Phi(lavaan_fit, num_vars)
      est_Psi     <- extract_lavaan_Psi(lavaan_fit, num_vars)
      VAR_est     <- list(Phi = est_Phi, Psi = est_Psi)
    } else {
      VAR_est     <- NULL 
    }

    # Remove lavaan_fit from output if `save_fit == FALSE`
    if (!save_fit) {
      lavaan_fit <- "[INFO] Model fit not saved; `save_fit' == FALSE in fit_lavaan_VAR()."
    }

    # Return output list
    return(list(lavaan_fit = lavaan_fit,
                warning_bool = warning_bool,
                error_bool = error_bool,
                one_cat_only = one_cat_only_warning,
                VAR_est = VAR_est))

}




#' Generates the lavaan syntax for a given VAR
#'
#' @param num_vars number of variables in the system
#' @param var_thres named list of thresholds for each of the ordinal variables
#' @param ordinal boolean indicating if the variables are ordinal
#'
#' @return a string in lavaan format for model fitting
gen_lavaan_dfm_syntax <- function(num_vars, var_thres = NULL, omit_thres = NULL, ordinal = TRUE) {

  # Set default for omit_thres
  if (is.null(omit_thres)) {
    omit_thres <- rep(FALSE, num_vars)
  }

  # Generate names
  varnames        <- paste0("V", seq(1, num_vars))

  # Check if var_thres are given for ordinal == TRUE
  if (ordinal) {
    if (is.null(var_thres)) {
      warning("No thresholds provided. Using default behavior in lavaan.")
    } else {
      # Verify the structure of the thresholds
      # We assume that the thresholds are provided in a named list
      # names should correspond to the variables
      stopifnot(all(names(var_thres) %in% varnames) && all(varnames %in% names(var_thres)))
      stopifnot(is.list(var_thres))
    }
  } else {
    if (!is.null(var_thres)) {
      stop("[ERROR] Thresholds should not be provided for continuous data.")
    }
  }

  # Generate names for the lagged variables
  lagged_varnames <- vector("list", 2)
  lagged_varnames[["lag-0"]] <- paste0(varnames, "_lag_0")
  lagged_varnames[["lag-1"]] <- paste0(varnames, "_lag_1")

  # Initialize main model string
  model_str <- "\n# Observed Ordinal VAR \n"


  ### --- Lag-0 equations as with lag-0 as DV --- ###

  # Add header for dummy zero section
  lag_0_reg_header  <- "\n# Autoregressive and cross-lagged relations \n"
  model_str         <- paste0(model_str, lag_0_reg_header)

  # Regress lag-0 on lag-1
  for (lag_0_dv in lagged_varnames[["lag-0"]]) {

    # Generate regression equation
    lag_1_ivs       <- paste0(lagged_varnames[["lag-1"]], collapse = " + ")
    lag_0_reg_eqn   <- paste0(c(lag_0_dv, lag_1_ivs), collapse = " ~ ")
    lag_0_reg_eqn   <- paste0(lag_0_reg_eqn, "\n")

    # Update main model string
    model_str <- paste0(model_str, lag_0_reg_eqn)
  }


  ### --- Lag-1 dummy equations --- ###

  # Add header for dummy zero section
  lag_1_reg_header  <- "\n# Dummy equations to treat lag-1 variables as endogenous \n"
  model_str         <- paste0(model_str, lag_1_reg_header)

  # Regress lag-1 on lag-0 with dummy zeros
  for (lag_1_dv in lagged_varnames[["lag-1"]]) {

    # Generate regression equation
    dummy_iv        <- paste0("0 * ", gsub("lag_1", "lag_0", lag_1_dv))
    lag_1_reg_eqn   <- paste0(c(lag_1_dv, dummy_iv), collapse = " ~ ")
    lag_1_reg_eqn   <- paste0(lag_1_reg_eqn, "\n")

    # Update main model string
    model_str <- paste0(model_str, lag_1_reg_eqn)

  }


  ### --- Lag-0 residual covariances --- ###
  lag_0_cov_header  <- "\n# Residual covariances \n"
  model_str         <- paste0(model_str, lag_0_cov_header)

  # Lag-0 residual covariances
  for (lag_0_cov_i in 1:(num_vars-1)) {
    for (lag_0_cov_j in (lag_0_cov_i+1):num_vars) {

      # Use upper triangular values
      dv_cov_eqn      <- lagged_varnames[["lag-0"]][lag_0_cov_i]
      iv_cov_eqn      <- lagged_varnames[["lag-0"]][lag_0_cov_j]
      lag_0_cov_eqn   <- paste0(c(dv_cov_eqn, iv_cov_eqn), collapse = " ~~ ")
      lag_0_cov_eqn   <- paste0(lag_0_cov_eqn, "\n")

      # Update main model string
      model_str <- paste0(model_str, lag_0_cov_eqn)
    }
  }


  ### --- Lag-1 covariances --- ###
  lag_1_cov_header  <- "\n# Empirical covariances \n"
  model_str         <- paste0(model_str, lag_1_cov_header)

  # Lag-1 residual covariances (i.e. empirical covariances)
  for (lag_1_cov_i in 1:(num_vars-1)) {
    for (lag_1_cov_j in (lag_1_cov_i+1):num_vars) {

      # Use upper triangular values
      dv_cov_eqn      <- lagged_varnames[["lag-1"]][lag_1_cov_i]
      iv_cov_eqn      <- lagged_varnames[["lag-1"]][lag_1_cov_j]
      lag_1_cov_eqn   <- paste0(c(dv_cov_eqn, iv_cov_eqn), collapse = " ~~ ")
      lag_1_cov_eqn   <- paste0(lag_1_cov_eqn, "\n")

      # Update main model string
      model_str <- paste0(model_str, lag_1_cov_eqn)
    }
  }


  ### --- Thresholds --- ###

  # Only implement this if ordinal and thresholds are provided
  if (ordinal && !is.null(var_thres)) {

    # Add thresholds header
    thresholds_header   <- "\n# Thresholds \n"
    model_str           <- paste0(model_str, thresholds_header)

    # Loop through variables and add threshold equations
    for (var_i in 1:num_vars) {

      if (omit_thres[var_i]) {

        # Add omission in text
        model_str <- paste0(model_str, paste0("# Omitted fixed thresholds for V", var_i, "\n"))

      } else {

        # Extract thresholds
        thres_var_i   <- var_thres[[varnames[var_i]]]
        thres_names   <- paste0("* t", seq(1, length(thres_var_i)))
        thres_eqn_iv  <- paste0(paste(thres_var_i, thres_names), collapse = " + ")

        # Create varnames for thresholds
        lag_0_thres_dv <- paste0(varnames[var_i], "_lag_0")
        lag_1_thres_dv <- paste0(varnames[var_i], "_lag_1")

        # Create equations
        lag_0_thres_eqn <- paste0(c(lag_0_thres_dv, thres_eqn_iv), collapse = " | ")
        lag_1_thres_eqn <- paste0(c(lag_1_thres_dv, thres_eqn_iv), collapse = " | ")
        lag_0_thres_eqn <- paste0(lag_0_thres_eqn, "\n")
        lag_1_thres_eqn <- paste0(lag_1_thres_eqn, "\n")

        # Update main model string
        model_str <- paste0(model_str, lag_0_thres_eqn)
        model_str <- paste0(model_str, lag_1_thres_eqn)
      }

    }
  }

  # End of model string
  model_str <- paste0(model_str, "\n# END MODEL STRING \n")

  # Return output
  return(model_str)
}



# Extracts Psi matrix for a fitted VAR model in lavaan
extract_lavaan_Psi <- function(lav_mod, num_vars) {

  # Extract only regressions from lavaan
  cov_only <- lavaan::parameterEstimates(lav_mod) %>%
    dplyr::filter(op == "~~")

  # Initialize
  psi_tmp <- matrix(NA, nrow = num_vars, ncol = num_vars)

  # Perform a double loop
  for (row_i in 1:num_vars) {
    for (col_j in row_i:num_vars) {

      # Create strings to be extracted
      row_str_i <- paste0("V", row_i, "_lag_0")
      col_str_j <- paste0("V", col_j, "_lag_0")

      # Fill up matrix with dplyr
      psi_tmp[row_i, col_j] <- cov_only %>%
        dplyr::filter(lhs == row_str_i & rhs == col_str_j) %>%
        dplyr::pull(est)

    }
  }

  # Fill in the lower triangular
  diag_psi_tmp            <- diag(diag(psi_tmp))
  t_psi_tmp               <- t(psi_tmp)
  diag(t_psi_tmp)         <- 0
  diag(psi_tmp)           <- 0

  # Zero out the NAs
  psi_tmp[is.na(psi_tmp)]       <- 0
  t_psi_tmp[is.na(t_psi_tmp)]   <- 0

  # Sum
  psi_out                 <- psi_tmp + t_psi_tmp + diag_psi_tmp

  return(psi_out)
}


# Extracts Phi matrix for a fitted VAR model in lavaan
extract_lavaan_Phi <- function(lav_mod, num_vars) {

  # Extract only regressions from lavaan
  reg_only <- lavaan::parameterEstimates(lav_mod) %>%
    dplyr::filter(op == "~")

  # Initialize
  phi_tmp <- matrix(NA, nrow = num_vars, ncol = num_vars)

  # Perform a double loop
  for (row_i in 1:num_vars) {
    for (col_j in 1:num_vars) {

      # Create strings to be extracted
      row_str_i <- paste0("V", row_i, "_lag_0")
      col_str_j <- paste0("V", col_j, "_lag_1")

      # Fill up matrix with dplyr
      phi_tmp[row_i, col_j] <- reg_only %>%
        dplyr::filter(lhs == row_str_i & rhs == col_str_j) %>%
        dplyr::pull(est)

    }
  }

  return(phi_tmp)
}

