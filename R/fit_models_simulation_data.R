# --- This code is to be used for simulation purposes only --- #
# We take in a list of (ntimepts x numvars) dataframes and 
# use mclapply to fit a lavaan model to the data
# we also extract the metrics of interest 

#' Function to run simulation for one timeseries 
#'
#' @param input_ts a dataframe of dimension (ntimepts x numvars)
#' @param timepts_vec vector of timepoints used for the simulations
#' @param ord_as_cont force ordinal variables to be continuous
#' @param estimator string indicating which estimator to use
run_simulation_one_ts <- function(input_ts,
                                  timepts_vec = c(50, 100, 200, 500, 1000),
                                  ord_as_cont = FALSE,
                                  estimator = "DEFAULT",
                                  save_fit = TRUE) {

    # Run checks
    max_timepts <- max(timepts_vec)
    stopifnot(nrow(input_ts) >= max_timepts)

    # Create named list to store estimated models
    fit_list            <- vector("list", length(timepts_vec))
    names(fit_list)     <- paste0("num_tps==", timepts_vec)

    # Loop through time points
    for (num_tps in timepts_vec) {

        # Truncate the actual time series accordingly
        trunc_ts <- input_ts[1:num_tps,]

        # Fit the model
        lav_fit_outlist <- fit_lavaan_VAR(trunc_ts,
                                          force_reorder = TRUE,
                                          ord_as_cont = ord_as_cont,
                                          estimator = estimator,
                                          save_fit = save_fit)
        lav_fit         <- lav_fit_outlist[["lavaan_fit"]]

        # Store parameters if fit succeeded
        if (!is.null(lav_fit)) {

            # Store estimated parameters 
            num_tps_string                  <- paste0("num_tps==", num_tps)
            fit_list[[num_tps_string]]      <- lav_fit_outlist

        }
        
        # Clear the truncated ts
        trunc_ts        <- NULL
        lav_fit_outlist <- NULL 
    }

    return(fit_list)
}


#' Function to mclapply over the different time series iterations
#' 
#' @param input_ts_list list of time-series from the same VAR model 
#' @param save_dir location to save the fitted models 
#' @param ord_as_cont force ordinal variables to be continuous
#' @param estimator string indicating which estimator to use
#' 
#' @export
run_simulation_multi_ts <- function(input_ts_list,
                                    save_dir = NULL,
                                    mc.cores = 4,
                                    timepts_vec = c(50, 100, 200, 500, 1000),
                                    ord_as_cont = FALSE,
                                    estimator = "DEFAULT",
                                    save_fit = TRUE) {
    
    # mclapply across iterations 
    mc_fit_list <- parallel::mclapply(input_ts_list, run_simulation_one_ts,
                                      timepts_vec = timepts_vec,
                                      ord_as_cont = ord_as_cont,
                                      estimator = estimator, 
                                      save_fit = save_fit,
                                      mc.cores = mc.cores)

    # If save.dir is specified
    if (!is.null(save_dir)) {

        # Create folder if necessary
        dir.create(save_dir, recursive = TRUE)

        # Save file 
        save_file <- paste0(save_dir, "/simulate_lavaan_VAR_fitted",
                            format(Sys.time(), "_%Y_%m_%d_%H%M%S"), ".RDS")
        saveRDS(mc_fit_list, file = save_file)

    }

    return(mc_fit_list)

}


