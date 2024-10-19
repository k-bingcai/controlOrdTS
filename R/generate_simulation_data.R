# --- This code is to be used for simulation purposes only --- #
# --- SPLIT THIS FILE INTO DATA GENERATION / MODEL FITTING / RESULT EXTRACTION --- #


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
#' 
#' @export
generate_TS_from_simobj <- function(sim_obj, num_mc_samples, max_timepts) {

    # Check that params have been created
    stopifnot(!is.null(sim_obj$saved_params))
    stopifnot(sim_obj$fixed_params == TRUE)

    # Initialize container 
    mc_ts_list  <- vector("list", num_mc_samples) 
    mc_mod_list <- vector("list", num_mc_samples)

    # mc.sample loop should be here
    for (mc_i in 1:num_mc_samples) {

        # Print out to console
        cat(paste0("Generating TS for Monte Carlo sample ", mc_i))

        # Generating TS data 
        gen_mod_i <- sim_obj$generate_ts_from_model(time_len = max_timepts)
        mc_mod_list[[mc_i]] <- gen_mod_i

        # # Extract relevant time series 
        # if (is.null(num.ord.out)) {
        #     mc_ts_list[[mc_i]] <- gen_mod_i$raw.ts
        # } else {
        #     mc_ts_list[[mc_i]] <- gen_mod_i$ord.ts[[as.character(num.ord.out)]]
        # }

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
    all_Phi_equal <- check_eq_params(sim_obj$saved_params, mc_mod_list,
                                     check.type = "Phi")
    all_Psi_equal <- check_eq_params(sim_obj$saved_params, mc_mod_list,
                                     check.type = "Psi")
    if (!is.null(sim_obj$num.ord)){
      all_thres_equal <- check_eq_params(sim_obj$saved_params, mc_mod_list,
                                         check.type = "raw.thres")
    } else {
      all_thres_equal <- TRUE
    }
    stopifnot(all_Phi_equal, all_Phi_equal, all_thres_equal)


    # We rearrange the time series generated into 
    # ordinf
    # +-- mc1
    # +-- mc2
    # ...
    # +-- mc100
    # ord7
    # +-- mc1
    # +-- mc2
    # ...
    # +-- mc100
    # ord5
    # +-- mc1
    # +-- mc2
    # Each mc_i uses the same underlying raw.ts
    ordinf_ts   <- lapply(mc_mod_list, function(x) x[["raw.ts"]])
    ord7_ts     <- lapply(mc_mod_list, function(x) x[["ord.ts"]][["7"]])
    ord5_ts     <- lapply(mc_mod_list, function(x) x[["ord.ts"]][["5"]])
    ord3_ts     <- lapply(mc_mod_list, function(x) x[["ord.ts"]][["3"]])
    out_list    <- list(ordinf = ordinf_ts,
                        ord7 = ord7_ts,
                        ord5 = ord5_ts,
                        ord3 = ord3_ts)

    return(out_list)
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
# |   +-- fitted (?)
# |   +-- results (?)