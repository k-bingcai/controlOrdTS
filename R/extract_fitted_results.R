# --- Functions to extract results from fitted models --- #

#' We need a function here that will extract the info from a list of (list of) fits
#' Outer list is for MC iterations
#' Inner list is for truncated timepoints
#' 
#' @export
extract_mc_results <- function(mc_fitted_list, true_Phi, true_Psi) {

    # Outer lapply over MC iterations
    output <- lapply(mc_fitted_list, collate_results_timelen,
                     true_Phi = true_Phi,
                     true_Psi = true_Psi)
    return(output)
}


#' Function to collate the VAR results for different time lengths
#' 
collate_results_timelen <- function(fitted_list, true_Phi, true_Psi) {

    # Extract results from each item in the list of timelengths
    list_of_results <- mapply(function(i, x) {
        # Extract results 
        tmp             <- compare_true_with_est(x, true_Phi, true_Psi)

        # Append time length to the output of each list 
        tmp$num_timepts <- tail(strsplit(i, split = "==")[[1]], 1)
        tmp
    }, names(fitted_list), fitted_list, SIMPLIFY = FALSE)

    # Combine into data.frame
    # list_of_results
    return(do.call(rbind, list_of_results))

}

#' Function to compare estimated with true parameters       ( THIS IS FOR ONE TIME LENGTH ONLY; NEED TO LAPPLY )
compare_true_with_est <- function(lav_fitobj, true_Phi, true_Psi) {

    # Return NULL if no fit object provided
    if (is.null(lav_fitobj)) {
        return(NULL)
    }

    # Create list of true parameters
    true_params <-  list(Phi = true_Phi,
                         Psi = true_Psi)

    # Initialize table of metrics
    metrics_df <- data.frame(phi_mse = NaN,
                             psi_mse = NaN,
                             gramian_sp_rank_corr = NaN,
                             est_correct_max_node = NaN,
                             est_max_in_top20p_nodes = NaN,
                             warning_bool = NaN,
                             error_bool = NaN,
                             one_cat_only = NaN)
    # metrics_df <- cbind(metrics_iden, metrics_df)

    # Extract fitted parameters 
    est_params <- lav_fitobj$VAR_est   
    
    # Ignore estimates if error 
    if (is.null(est_params)) {

        if (!lav_fitobj$error_bool) {
            stop("[ERROR] Estimated parameters is NULL but no error in lavaan fit.")
        }

        return(metrics_df)
    }

    # Compute metrics of interest 
    VAR_metrics <- compute_VAR_metrics(true_params, est_params)

    # Store metrics
    metrics_df$phi_mse                 <- VAR_metrics[["mse_Phi"]]
    metrics_df$psi_mse                 <- VAR_metrics[["mse_Psi"]]
    metrics_df$gramian_sp_rank_corr    <- VAR_metrics[["gramian_sp_rank_corr"]]
    metrics_df$est_correct_max_node    <- VAR_metrics[["est_correct_max_node"]]
    metrics_df$est_max_in_top20p_nodes <- VAR_metrics[["est_max_in_top20p_nodes"]]

    # Extract warning and errors
    metrics_df$warning_bool          <- lav_fitobj[["warning_bool"]]
    metrics_df$error_bool            <- lav_fitobj[["error_bool"]]
    metrics_df$one_cat_only          <- lav_fitobj[["one_cat_only"]]

    return(metrics_df)

}


#' Function to compute metrics of interest given lavaan fit and true values
#'
#' @param true_params
#' @param est_params
compute_VAR_metrics <- function(true_params, est_params) {

    # Extract true parameters
    true_Phi <- true_params[["Phi"]]
    true_Psi <- true_params[["Psi"]]

    # Extract estimated parameters
    est_Phi <- est_params[["Phi"]]
    est_Psi <- est_params[["Psi"]]

    # MSE
    mse_Phi <- mean((true_Phi - est_Phi) ** 2)
    mse_Psi <- mean((true_Psi - est_Psi) ** 2)

    # Gramian
    true_diag_gramian   <- diag(netcontrol::control_gramian(A = true_Phi,
                                                            B = diag(dim(true_Phi)[1])))
    est_diag_gramian    <- diag(netcontrol::control_gramian(A = est_Phi,
                                                            B = diag(dim(est_Phi)[1])))

    # Spearman rank correlation
    gramian_sp_rank_corr <- cor.test(x = true_diag_gramian,
                                     y = est_diag_gramian,
                                     method = "spearman")$estimate

    # Most important control node (from true gramian)
    true_max_nodes        <- which(true_diag_gramian == max(true_diag_gramian))
    true_top20p_nodes     <- which(true_diag_gramian %in% sort(true_diag_gramian, decreasing = TRUE)[1:round(length(true_diag_gramian)/ 5)])

    # Most important control node (estimated)
    est_max_nodes           <- which(est_diag_gramian == max(est_diag_gramian))
    est_correct_max_node    <- any(est_max_nodes %in% true_max_nodes)     # We use any in case of ties
    est_max_in_top20p_nodes <- any(est_max_nodes %in% true_top20p_nodes)

    # Output named list
    return(list(mse_Phi = mse_Phi,
                mse_Psi = mse_Psi,
                gramian_sp_rank_corr = gramian_sp_rank_corr,
                est_correct_max_node = est_correct_max_node,
                est_max_in_top20p_nodes = est_max_in_top20p_nodes,
                est_diag_gramian = est_diag_gramian))    

}