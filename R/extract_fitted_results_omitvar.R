
# --- Functions to extract results from fitted models --- #

#' We need a function here that will extract the info from a list of (list of) fits
#' Outer list is for MC iterations
#' Inner list is for truncated timepoints
#' 
#' @export
extract_mc_results_omitvar <- function(mc_fitted_list, true_Phi, true_Psi,
                                       omitted_var_index) {

    # Outer lapply over MC iterations
    output <- lapply(mc_fitted_list, collate_results_timelen_omitvar,
                     true_Phi = true_Phi,
                     true_Psi = true_Psi,
                     omitted_var_index = omitted_var_index)
    return(output)
}


#' Function to collate the VAR results for different time lengths
#' 
collate_results_timelen_omitvar <- function(fitted_list, true_Phi, true_Psi,
                                            omitted_var_index) {

    # Extract results from each item in the list of timelengths
    list_of_results <- mapply(function(i, x) {
        # Extract results 
        tmp             <- compare_true_with_est_omitvar(x, true_Phi, true_Psi,
                                                         omitted_var_index)

        # Append time length to the output of each list 
        tmp$num_timepts <- tail(strsplit(i, split = "==")[[1]], 1)
        tmp
    }, names(fitted_list), fitted_list, SIMPLIFY = FALSE)

    # Combine into data.frame
    # list_of_results
    return(do.call(rbind, list_of_results))

}



#' Function to compare estimated with true parameters       ( THIS IS FOR ONE TIME LENGTH ONLY; NEED TO LAPPLY )
compare_true_with_est_omitvar <- function(lav_fitobj, true_Phi, true_Psi, 
                                          omitted_var_index) {

    # Return NULL if no fit object provided
    if (is.null(lav_fitobj)) {
        return(NULL)
    }

    # Create list of true parameters
    true_params <-  list(Phi = true_Phi,
                         Psi = true_Psi)

    # Initialize table of metrics
    metrics_df <- data.frame(gramian_sp_rank_corr_omitvar = NaN,
                             est_correct_max_node_omitvar = NaN,
                             est_max_in_top20p_nodes_omitvar = NaN,
                             mse_across_nodes = NaN,
                             warning_bool = NaN,
                             error_bool = NaN,
                             one_cat_only = NaN)

    # Extract fitted parameters 
    est_params <- lav_fitobj$VAR_est   
    
    # Ignore estimates if error 
    if (is.null(est_params)) {

        if (!lav_fitobj$error_bool) {
            stop("[ERROR] Estimated parameters is NULL but no error in lavaan fit.")
        }

        # Extract warning and errors
        metrics_df$warning_bool          <- lav_fitobj[["warning_bool"]]
        metrics_df$error_bool            <- lav_fitobj[["error_bool"]]
        metrics_df$one_cat_only          <- lav_fitobj[["one_cat_only"]]

        return(metrics_df)
    }

    # Compute metrics of interest 
    VAR_metrics <- compute_VAR_metrics_omitvar(true_params, est_params,
                                               omitted_var_index = omitted_var_index)

    # Store metrics
    metrics_df$gramian_sp_rank_corr_omitvar    <- VAR_metrics[["gramian_sp_rank_corr"]]
    metrics_df$est_correct_max_node_omitvar    <- VAR_metrics[["est_correct_max_node"]]
    metrics_df$est_max_in_top20p_nodes_omitvar <- VAR_metrics[["est_max_in_top20p_nodes"]]
    metrics_df$mse_across_nodes                <- VAR_metrics[["mse_across_nodes"]]

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
#' @param omitted_var_index
compute_VAR_metrics_omitvar <- function(true_params, est_params, omitted_var_index) {

    # Extract true parameters
    true_Phi <- true_params[["Phi"]]
    true_Psi <- true_params[["Psi"]]

    # Extract estimated parameters
    est_Phi <- est_params[["Phi"]]
    est_Psi <- est_params[["Psi"]]

    # Check that dimension of estimation is one less than true 
    stopifnot(dim(est_Phi)[1] == dim(true_Phi)[1] - 1)

    # Gramian
    true_diag_gramian   <- diag(netcontrol::control_gramian(A = true_Phi,
                                                            B = diag(dim(true_Phi)[1])))
    est_diag_gramian    <- diag(netcontrol::control_gramian(A = est_Phi,
                                                            B = diag(dim(est_Phi)[1])))

    # Insert missing gramian value 
    est_diag_gramian    <- insert_NA_in_position(omitted_var_index,
                                                 est_diag_gramian)

    # Spearman rank correlation
    gramian_sp_rank_corr <- cor.test(x = true_diag_gramian,
                                     y = est_diag_gramian,
                                     method = "spearman",
                                     na.action = "na.exclude")$estimate

    # Most important control node (from true gramian)
    tmp_true_diag_gramian_w_NA                      <- true_diag_gramian
    tmp_true_diag_gramian_w_NA[omitted_var_index]   <- NA
    true_max_nodes        <- which(tmp_true_diag_gramian_w_NA == max(tmp_true_diag_gramian_w_NA, na.rm = TRUE))
    top20p_nodes_vec      <- sort(tmp_true_diag_gramian_w_NA,
                                  decreasing = TRUE,
                                  na.last = TRUE)[1:round(length(tmp_true_diag_gramian_w_NA)/ 5)]
    true_top20p_nodes     <- which(true_diag_gramian %in% top20p_nodes_vec)

    # Most important control node (estimated)
    est_max_nodes           <- which(est_diag_gramian == max(est_diag_gramian, na.rm = TRUE))
    est_correct_max_node    <- any(est_max_nodes %in% true_max_nodes)     # We use any in case of ties
    est_max_in_top20p_nodes <- any(est_max_nodes %in% true_top20p_nodes)

    # Average estimation error across estimated nodes 
    mse_across_nodes <- mean((est_diag_gramian - true_diag_gramian) ** 2, na.rm = TRUE)

    # Output named list
    return(list(gramian_sp_rank_corr = gramian_sp_rank_corr,
                est_correct_max_node = est_correct_max_node,
                est_max_in_top20p_nodes = est_max_in_top20p_nodes,
                mse_across_nodes = mse_across_nodes,
                est_diag_gramian = est_diag_gramian))    

}


# Convenience function to insert NA at given index in a vector
insert_NA_in_position <- function(index, vec) {
    vec_len <- length(vec)
    stopifnot(index >= 1 && index <= vec_len + 1)
    if (index == 1) {
        out_vec <- c(NA, vec)
    } else if (index == vec_len + 1) {
        out_vec <- c(vec, NA)
    } else {
        front   <- vec[1:(index-1)] 
        end     <- vec[index:vec_len]
        out_vec <- c(front, NA, end)
    }
    return(out_vec)
}