# --- This code is to be used for simulation purposes only --- #
# --- SPLIT THIS FILE INTO DATA GENERATION / MODEL FITTING --- @


#' Creates simulation object based on simulation parameters
#'
#' @param phi_func
#' @param phi_func_args
#' @param num.ord.max
#'
#' @export
create_sim_obj <- function(phi.func, phi.func.args = list(), num.ord.max = 7) {

    # Create random matrix with eigenvalues of norm < 1 (i.e. in the unit circle)
    phi_m <- do.call("phi.func", c(phi.func.args))

    # Create random psi covariance matrix
    nvars <- nrow(phi_m)
    psi_m <- random_Psi(nvars)

    # Standardize the TS model 
    sVAR_params <- standardize_VAR(Phi_in = phi_m, Psi_in = psi_m)

    # Check if infinite categories were provided
    if (is.infinite(num.ord.max)) {
        if (sign(num.ord.max) == -1) {
        stop("[ERROR] argument `num.ord.max` cannot be -Inf.")
        } else {
        num.ord_in <- NULL
        }
    } else {
        num.ord_in <- num.ord.max
    }

    # Construct object and initialize
    sim_obj <- simulateVAR$new(Phi = sVAR_params$Phi,
                               Psi = sVAR_params$Psi,
                               num.ord.max = num.ord_in,
                               fixed_params = TRUE)
    sim_obj$initialize_models()

    # Set custom name and tags
    sim_obj$set_custom_name(as.character(substitute(phi.func)))
    sim_obj$set_custom_tags(list(phi.func.args = phi.func.args,
                                 num.ord.max = num.ord.max))

    return(sim_obj)
}

#' Generate multiple TS from a given simulation object
generate_TS_from_simobj <- function(sim_obj, num_mc_samples, max_timepts, num.ord.out = NULL) {

    # Check that params have been created
    stopifnot(!is.null(sim_obj$saved_params))
    stopifnot(sim_obj$fixed_params == TRUE)

    # Initialize container 
    mc_ts_list <- vector("list", num_mc_samples) 

    # mc.sample loop should be here
    for (mc_i in 1:num_mc_samples) {

        # Print out to console
        cat(paste0("Generating TS for Monte Carlo sample ", mc_i))

        # Generating TS data 
        mc_ts_list[[mc_i]] <- sim_obj$generate_ts_from_model(time_len = max_timepts,
                                                             num.ord.out = num.ord.out)

    }

    # Sanity check that all parameters are the same across mc samples
    check_eq_params <- function(ref_params, params_list_to_check,
                                check.type = c("Phi", "Psi", "raw.thres")) {
        check.type      <- match.arg(check.type)
        ref_params_list <- replicate(length(params_list_to_check),
                                     ref_params, 
                                     simplify = FALSE)
        all_params_eq   <- all(mapply(function(x,y) {all(x[[check.type]] == y[[check.type]])},
                                      x = ref_params_list,
                                      y = params_list_to_check))
        return(all_params_eq)
    }
    all_Phi_equal <- check_eq_params(sim_obj$saved_params, mc_ts_list,
                                     check.type = "Phi")
    all_Psi_equal <- check_eq_params(sim_obj$saved_params, mc_ts_list,
                                     check.type = "Psi")
    if (!is.null(sim_obj$num.ord)){
      all_thres_equal <- check_eq_params(sim_obj$saved_params, mc_ts_list,
                                         check.type = "raw.thres")
    } else {
      all_thres_equal <- TRUE
    }
    stopifnot(all_Phi_equal, all_Phi_equal, all_thres_equal)

    return(mc_ts_list)
}


# Save the generated TS so that we can run different estimators on it 
# We should mclapply over the 500 iterations
# Then send each model / person as a separate job (i.e. 100 jobs)
# Previously, we were doing mclapply across people

# For each model, generate 500 TS, save this as RDS
# bringmann_2017_dataset1
# +-- model_1
# |   +-- simobj
# |   |   +-- simobj_model_1.RDS
# |   +-- data
# |   |   +-- TS_list_model_1_ordinf.RDS
# |   |   +-- TS_list_model_1_ord7.RDS
# |   |   +-- TS_list_model_1_ord5.RDS
# |   |   +-- TS_list_model_1_ord3.RDS
# |   +-- results (?)