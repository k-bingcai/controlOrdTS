# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

#' Function to extract simulation results
#'
#' @param sim_dir simulation directory
#' @param skeleton name of skeleton to plot for
extract_sim_results <- function(sim_dir,
                                skeleton,
                                include_ordascont = TRUE,
                                cols_to_extract = "all") {

  # Specify ord classes
  ord_cls <- c("ord3", "ord5", "ord7", "ordinf")

  # Check if directories exist
  skeleton_dir <- paste(sim_dir, "models", skeleton, sep = "/")
  stopifnot(file.exists(skeleton_dir))

  # Initialize main df
  main_df <- NULL
  miss_df <- NULL

  # Additional loop if we include ordascont
  if (include_ordascont) {
    ordascont_loopvars <- c("_results.RDS", "_results_ordascont.RDS")
  } else {
    ordascont_loopvars <- c("_results.RDS")
  }

  # Loop through models in skeleton_dir
  mod_files <- list.files(skeleton_dir)
  for (mod_i in mod_files) {

    # Create string to read file from results
    for (cls in ord_cls) {

      # Extra loop for ordascont
      for (oac in ordascont_loopvars) {

        # Specify file paths
        res_i_cls_filename <- paste("TS_list_", mod_i, "_", cls, oac,
                                    sep = "")
        res_i_cls_filepath <- paste(skeleton_dir,
                                    mod_i,
                                    "results",
                                    res_i_cls_filename,
                                    sep = "/")

        # Read the results file
        if (file.exists(res_i_cls_filepath)) {
          res_i_cls_RDS_raw <- readRDS(res_i_cls_filepath)
        } else {
          cat(paste0("[WARNING] ", res_i_cls_filepath, " is missing\n"))
          next
        }

        # Filter out missing fits
        res_i_cls_RDS <- Filter(function(x) !is.null(x), res_i_cls_RDS_raw)
        res_i_cls_miss <- data.frame(mod_i = mod_i,
                                     ord_cls = cls,
                                     num_miss = length(res_i_cls_RDS_raw) - length(res_i_cls_RDS),
                                     num_total = length(res_i_cls_RDS_raw))

        # Compute average over MC samples
        if (!identical(cols_to_extract, "all")) {
          stopifnot("num_timepts" %in% cols_to_extract)
          tmp_res <- lapply(res_i_cls_RDS, function(z) subset(z, select = cols_to_extract))
        } else {
          tmp_res <- res_i_cls_RDS
        }
        tmp_res <- lapply(tmp_res,
                          function(x) as.matrix(subset(x, select = -c(num_timepts))))
        tmp_res <- simplify2array(tmp_res)
        avg_res <- apply(tmp_res, 1:2, mean, na.rm = TRUE)
        avg_res <- as.data.frame(avg_res)

        # Add back number of timepoints from row names
        avg_res$num_timepts <- as.integer(unlist(lapply(strsplit(rownames(avg_res), split = "=="),
                                                        function(x) x[2])))
        rownames(avg_res) <- NULL

        # Add ordascont indicator to data frame
        if (oac == "_results.RDS") {
          avg_res$ordascont <- FALSE
        } else if (oac == "_results_ordascont.RDS") {
          avg_res$ordascont <- TRUE
        } else {
          stop("[ERROR] Invalid `oac` value.")
        }

        # Append to main dataframe (mod_i, ord_cls)
        avg_res$mod_i   <- mod_i
        avg_res$ord_cls <- cls
        main_df <- rbind(main_df, avg_res)
        miss_df <- rbind(miss_df, res_i_cls_miss)

      }

    }

  }

  # Return
  return(list(main = main_df,
              miss = miss_df,
              skeleton = skeleton))

}




# Plot boxplots as a function of time
plot_boxplots <- function(column, data_to_plot, log_y = FALSE, ordascont_val = FALSE) {

  print("[WARNING] y-axis is not constrained to be the same for different ordascont values")

  if (log_y) {
    plot_obj <- ggplot(data_to_plot$main %>% filter(ordascont == ordascont_val),
                       aes(x = num_timepts, y = log(!!sym(column)),
                           group = num_timepts, fill = log(num_timepts)))
  } else {
    plot_obj <- ggplot(data_to_plot$main %>% filter(ordascont == ordascont_val),
                       aes(x = num_timepts, y = !!sym(column),
                           group = num_timepts, fill = log(num_timepts)))
  }
  plot_obj +
    geom_boxplot(outlier.size = 0.5) +
    scale_fill_viridis_c(option = "magma", begin = 0.6) +
    facet_grid(~ord_cls) +
    xlab("Number of Timepoints") +
    scale_x_continuous(limits = c(0, 1050),
                       breaks = c(100, 250, 500, 1000)) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(data_to_plot$skeleton)

}

