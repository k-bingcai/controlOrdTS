# --- Code for plotting --- #
library(ggplot2)
library(tidyr)
library(dplyr)

#' Function to extract simulation results
#'
#' @param sim_dir simulation directory
#' @param skeleton name of skeleton to plot for
extract_sim_results <- function(sim_dir,
                                skeleton,
                                include_ordascont = TRUE) {

  # Specify ord classes
  ord_cls <- c("ord3", "ord5", "ord7", "ordinf")

  # Check if dierctories exist
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
        tmp_res <- lapply(res_i_cls_RDS,
                          function(x) as.matrix(subset(x, select = -c(num_timepts))))
        tmp_res <- simplify2array(tmp_res)
        avg_res <- apply(tmp_res, 1:2, mean)
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


# Extract simulations
sim_date   <- "20241111_225322"
sim_folder <- "/home/bing/Desktop/UNC-CH/projects/controlOrdTS_all/resources/simulations/"

# Extract bringmann_1
bringmann_1_out <- extract_sim_results(paste0(sim_folder, sim_date),
                                       skeleton = "bringmann_2017_dataset1")

# Extract bringmann_2
bringmann_2_out <- extract_sim_results(paste0(sim_folder, sim_date),
                                       skeleton = "bringmann_2017_dataset2")

# Extract for scale-free
scale_free_out <- extract_sim_results(paste0(sim_folder, sim_date),
                                      skeleton = "scale_free_seeded")


# Plot differences between ordascont conditions
plot_diff <- function(column, data_to_plot) {

  # Get difference in column
  diff_plot_data <- data_to_plot$main %>%
    select(!!sym(column), mod_i, num_timepts, ord_cls, ordascont) %>%
    pivot_wider(names_from = ordascont, values_from = !!sym(column), names_prefix = "ordascont.") %>%
    mutate(ordascont_diff = ordascont.FALSE - ordascont.TRUE)

  # Create plot
  ggplot(diff_plot_data,
         aes(x = num_timepts, y = ordascont_diff,
             group = num_timepts, fill = log(num_timepts))) +
    geom_boxplot(outlier.size = 0.5) +
    geom_hline(yintercept = 0, lty = 3, alpha = 0.4) +
    scale_fill_viridis_c(option = "magma", begin = 0.6) +
    facet_grid(~ord_cls) +
    xlab("Number of Timepoints") +
    ylab("<-- (ML)          (DWLS) -->") +
    scale_x_continuous(limits = c(0, 1050),
                       breaks = c(100, 250, 500, 1000)) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(paste(data_to_plot$skeleton, "(", column, ")"))


}


# Plot histogram as a function of time
plot_bars <- function(column, data_to_plot, log_y = FALSE, ordascont_val = FALSE) {

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

# Plot the results
# Bringmann 1
plot_bars("phi_mse", bringmann_1_out, log_y = TRUE)
plot_bars("psi_mse", bringmann_1_out, log_y = TRUE)
plot_bars("est_correct_max_node", bringmann_1_out)
plot_bars("est_max_in_top20p_nodes", bringmann_1_out)
plot_bars("gramian_sp_rank_corr", bringmann_1_out)
plot_diff("est_correct_max_node", bringmann_1_out)
plot_diff("est_max_in_top20p_nodes", bringmann_1_out)
plot_diff("gramian_sp_rank_corr", bringmann_1_out)


# Bringmann 2
plot_bars("phi_mse", bringmann_2_out, log_y = TRUE)
plot_bars("psi_mse", bringmann_2_out, log_y = TRUE)
plot_bars("est_correct_max_node", bringmann_2_out)
plot_bars("est_max_in_top20p_nodes", bringmann_2_out)
plot_bars("gramian_sp_rank_corr", bringmann_2_out)
plot_diff("est_correct_max_node", bringmann_2_out)
plot_diff("est_max_in_top20p_nodes", bringmann_2_out)
plot_diff("gramian_sp_rank_corr", bringmann_2_out)


# Scale free
plot_bars("phi_mse", scale_free_out, log_y = TRUE)
plot_bars("psi_mse", scale_free_out, log_y = TRUE)
plot_bars("est_correct_max_node", scale_free_out)
plot_bars("est_max_in_top20p_nodes", scale_free_out)
plot_bars("gramian_sp_rank_corr", scale_free_out)
plot_diff("est_correct_max_node", scale_free_out)
plot_diff("est_max_in_top20p_nodes", scale_free_out)
plot_diff("gramian_sp_rank_corr", scale_free_out)

# combined_plot <- ggplot(z$main,
#                         aes(x = num_timepts, y = est_correct_max_node,
#                             group = num_timepts, fill = log(num_timepts))) + # , color = mod_i)) +
#   geom_boxplot() +
#   scale_fill_viridis_c(option = "magma", begin = 0.6) +
#   # geom_line(alpha = 0.6,
#   #           position=position_jitter(w=0.00, h=0.05)) +
#   # ggtitle(plot_title) +
#   facet_grid(~ord_cls) + # ,
#   # labeller = labeller(numord = create_ord_plot_label,
#   #                     syssize = create_syssize_plot_label)) +
#   # ylab(plot_ylab) +
#   xlab("Number of Timepoints") +
#   scale_x_continuous(limits = c(0, 1050),
#                      breaks = c(100, 250, 500, 1000)) +
#   theme_bw() +
#   theme(legend.position="none")
# combined_plot
#
#
#
# # library(ggplot2)
# # combined_plot <- ggplot(z$main,
# #                         aes(x = num_timepts, y = est_correct_max_node, color = mod_i)) +
# #   geom_line(alpha = 0.6,
# #             position=position_jitter(w=0.00, h=0.05)) +
# #   # ggtitle(plot_title) +
# #   facet_grid(~ord_cls) + # ,
# #              # labeller = labeller(numord = create_ord_plot_label,
# #              #                     syssize = create_syssize_plot_label)) +
# #   # ylab(plot_ylab) +
# #   xlab("Number of Timepoints") +
# #   scale_x_continuous(limits = c(0, 1000),
# #                      breaks = c(100, 250, 500, 1000)) +
# #   theme_bw() +
# #   theme(legend.position="none")
# # combined_plot
#
#
#
#
# combined_plot <- ggplot(z$main,
#                         aes(x = num_timepts, y = est_correct_max_node,
#                             group = num_timepts, fill = log(num_timepts))) + # , color = mod_i)) +
#   geom_boxplot() +
#   scale_fill_viridis_c(option = "magma", begin = 0.6) +
#   # geom_line(alpha = 0.6,
#   #           position=position_jitter(w=0.00, h=0.05)) +
#   # ggtitle(plot_title) +
#   facet_grid(~ord_cls) + # ,
#   # labeller = labeller(numord = create_ord_plot_label,
#   #                     syssize = create_syssize_plot_label)) +
#   # ylab(plot_ylab) +
#   xlab("Number of Timepoints") +
#   scale_x_continuous(limits = c(0, 1050),
#                      breaks = c(100, 250, 500, 1000)) +
#   theme_bw() +
#   theme(legend.position="none")
# combined_plot
#
#
#
#
# combined_plot <- ggplot(z$main,
#                         aes(x = num_timepts, y = est_max_in_top20p_nodes,
#                             group = num_timepts, fill = log(num_timepts))) + # , color = mod_i)) +
#   geom_boxplot() +
#   scale_fill_viridis_c(option = "magma", begin = 0.6) +
#   # geom_line(alpha = 0.6,
#   #           position=position_jitter(w=0.00, h=0.05)) +
#   # ggtitle(plot_title) +
#   facet_grid(~ord_cls) + # ,
#   # labeller = labeller(numord = create_ord_plot_label,
#   #                     syssize = create_syssize_plot_label)) +
#   # ylab(plot_ylab) +
#   xlab("Number of Timepoints") +
#   scale_x_continuous(limits = c(0, 1050),
#                      breaks = c(100, 250, 500, 1000)) +
#   theme_bw() +
#   theme(legend.position="none")
# combined_plot
#
#
#
# combined_plot <- ggplot(z$main,
#                         aes(x = num_timepts, y = gramian_sp_rank_corr,
#                             group = num_timepts, fill = log(num_timepts))) + # , color = mod_i)) +
#   geom_boxplot() +
#   scale_fill_viridis_c(option = "magma", begin = 0.6) +
#   # geom_line(alpha = 0.6,
#   #           position=position_jitter(w=0.00, h=0.05)) +
#   # ggtitle(plot_title) +
#   facet_grid(~ord_cls) + # ,
#   # labeller = labeller(numord = create_ord_plot_label,
#   #                     syssize = create_syssize_plot_label)) +
#   # ylab(plot_ylab) +
#   xlab("Number of Timepoints") +
#   scale_x_continuous(limits = c(0, 1050),
#                      breaks = c(100, 250, 500, 1000)) +
#   theme_bw() +
#   theme(legend.position="none")
# combined_plot

