# --- Functions to extract results from fitted models for pre-post comparisons --- #

#' We need a function here that will extract the info from a list of (list of) fits
#' Outer list is for MC iterations
#' Inner list is for truncated timepoints
#' 
#' @export
extract_mc_results_prepost <- function(mc_pre_fitted_list, mc_post_fitted_list,
                                       pre_true_Phi, post_true_Phi) {

    # Outer lapply over MC iterations
    output <- mapply(function(x,y) {
        collate_results_timelen_prepost(x, y, 
                                        pre_true_Phi, post_true_Phi)
    }, mc_pre_fitted_list, mc_post_fitted_list, SIMPLIFY = FALSE)
    return(output)

}


#' Function to collate the VAR results for different time lengths
#' 
collate_results_timelen_prepost <- function(pre_fitted_list, post_fitted_list,
                                            pre_true_Phi, post_true_Phi) {

    # Sanity check 
    stopifnot(names(pre_fitted_list) == names(post_fitted_list))

    # Extract results from each item in the list of timelengths
    list_of_results <- mapply(function(i, x, y) {
        # Extract results 
        tmp             <- compare_prepost(x, y, 
                                           pre_true_Phi, post_true_Phi)

        # Append time length to the output of each list 
        tmp$num_timepts <- tail(strsplit(i, split = "==")[[1]], 1)
        tmp
    }, names(pre_fitted_list), pre_fitted_list, post_fitted_list, SIMPLIFY = FALSE)

    # Combine into data.frame
    # list_of_results
    return(do.call(rbind, list_of_results))

}


#' Function to compare prepost
compare_prepost <- function(pre_lav_fitobj, post_lav_fitobj,
                            pre_true_Phi, post_true_Phi) {

    # Initialize table of metrics
    metrics_df <- data.frame(true_change_most_controllable_node = NaN,
                            true_pre_most_controllable_node = NaN,
                            true_post_most_controllable_node = NaN,
                            true_post_minus_pre_trace_gramian = NaN,
                            true_prepost_gramian_sp_rank_corr = NaN,
                            est_change_most_controllable_node = NaN,
                            est_pre_most_controllable_node = NaN,
                            est_post_most_controllable_node = NaN,
                            est_post_minus_pre_trace_gramian = NaN,
                            est_prepost_gramian_sp_rank_corr = NaN,
                            post_minus_pre_trace_gramian_rel_bias = NaN,
                            diff_prepost_gramian_sp_rank_corr = NaN,
                            warning_bool = NaN,
                            error_bool = NaN,
                            one_cat_only = NaN)

    # Extract fitted parameters
    pre_est_params <- pre_lav_fitobj$VAR_est
    post_est_params <- post_lav_fitobj$VAR_est

    # Ignore estimates if error
    if (is.null(pre_est_params) || is.null(post_est_params)) {

        if (!pre_lav_fitobj$error_bool && !post_lav_fitobj$error_bool) {    # This should be an && 
            stop("[ERROR] Estimated parameters is NULL but no error in lavaan fit.")
        }

        # Extract warning and errors
        metrics_df$warning_bool          <- any(pre_lav_fitobj[["warning_bool"]], post_lav_fitobj[["warning_bool"]])
        metrics_df$error_bool            <- any(pre_lav_fitobj[["error_bool"]], post_lav_fitobj[["error_bool"]])
        metrics_df$one_cat_only          <- any(pre_lav_fitobj[["one_cat_only"]], post_lav_fitobj[["one_cat_only"]])

        return(metrics_df)
    }

    # True Gramian
    pre_true_diag_gramian   <- diag(netcontrol::control_gramian(A = pre_true_Phi,
                                                                B = diag(dim(pre_true_Phi)[1])))
    post_true_diag_gramian  <- diag(netcontrol::control_gramian(A = post_true_Phi,
                                                                B = diag(dim(post_true_Phi)[1])))

    # Compute true values for each metric
    true_metrics <- compute_prepost_gramian_metrics(pre_gramian = pre_true_diag_gramian,
                                                    post_gramian = post_true_diag_gramian)

    # Extract estaimted Phi matrices
    pre_est_Phi   <- pre_est_params$Phi
    post_est_Phi  <- post_est_params$Phi

    # Estimated Gramians
    pre_est_diag_gramian   <- diag(netcontrol::control_gramian(A = pre_est_Phi,
                                                                B = diag(dim(pre_est_Phi)[1])))
    post_est_diag_gramian  <- diag(netcontrol::control_gramian(A = post_est_Phi,
                                                                B = diag(dim(post_est_Phi)[1])))

    # Compute true values for each metric
    est_metrics <- compute_prepost_gramian_metrics(pre_gramian = pre_est_diag_gramian,
                                                    post_gramian = post_est_diag_gramian)

    # Append both true and estimated
    colnames(true_metrics) <- paste("true", colnames(true_metrics), sep = "_")
    colnames(est_metrics) <- paste("est", colnames(est_metrics), sep = "_")
    all_metrics <- cbind(true_metrics, est_metrics)

    # Compute difference / rel bias
    all_metrics <- all_metrics %>%
        dplyr::mutate(post_minus_pre_trace_gramian_rel_bias =
                abs(true_post_minus_pre_trace_gramian - est_post_minus_pre_trace_gramian) /
                abs(true_post_minus_pre_trace_gramian)) %>%
        dplyr::mutate(diff_prepost_gramian_sp_rank_corr =
                        true_prepost_gramian_sp_rank_corr - est_prepost_gramian_sp_rank_corr)

    # Extract warning and errors
    metrics_df$warning_bool          <- any(pre_lav_fitobj[["warning_bool"]], post_lav_fitobj[["warning_bool"]])
    metrics_df$error_bool            <- any(pre_lav_fitobj[["error_bool"]], post_lav_fitobj[["error_bool"]])
    metrics_df$one_cat_only          <- any(pre_lav_fitobj[["one_cat_only"]], post_lav_fitobj[["one_cat_only"]])

    # Update and check entries in all_metrics and metrics_df
    stopifnot(colnames(all_metrics) %in% colnames(metrics_df))
    for (metric_name in colnames(all_metrics)) {
        metrics_df[[metric_name]] <- all_metrics[[metric_name]]
    }
    return(metrics_df)

}


#' Compute pre-post metrics for gramian
compute_prepost_gramian_metrics <- function(pre_gramian, post_gramian) {

  # Initialize table of metrics
  metrics_df <- data.frame(change_most_controllable_node = NaN,
                           pre_most_controllable_node = NaN,
                           post_most_controllable_node = NaN,
                           post_minus_pre_trace_gramian = NaN,
                           prepost_gramian_sp_rank_corr = NaN)

  #' Convenience function to extract max node(s)
  max_nodes <- function(node_vec) {
    which(node_vec == max(node_vec))
  }

  # Store metrics
  metrics_df$change_most_controllable_node      <- !(identical(max_nodes(pre_gramian), max_nodes(post_gramian)))
  metrics_df$pre_most_controllable_node         <- paste(as.character(max_nodes(pre_gramian)), collapse = ",")
  metrics_df$post_most_controllable_node        <- paste(as.character(max_nodes(post_gramian)), collapse = ",")
  metrics_df$post_minus_pre_trace_gramian       <- sum(post_gramian) - sum(pre_gramian)
  metrics_df$prepost_gramian_sp_rank_corr       <- cor.test(x = pre_gramian,
                                                            y = post_gramian,
                                                            method = "spearman")$estimate

  return(metrics_df)

}
