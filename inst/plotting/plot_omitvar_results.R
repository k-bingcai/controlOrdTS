# Plotting for omitting variables

# Clear environment
rm(list=ls())
cat("\014")

# Source functions
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("plot_common.R")

# Specifics
sim_date <- "20241126_231346"
sim_folder <- "/home/bing/Desktop/UNC-CH/projects/controlOrdTS_all/resources/simulations/"

# Extract data
bringmann_2_omitvar_out <- extract_sim_results(paste0(sim_folder, sim_date),
                                               skeleton = "bringmann_2017_dataset2",
                                               omitvar_only = TRUE)

bringmann_2_nonomit_out <- extract_sim_results(paste0(sim_folder, sim_date),
                                               skeleton = "bringmann_2017_dataset2",
                                               omitvar_only = FALSE)


# Fix the max node computation (DONE)
# Merge this with non-omitted results
# Subtract and form plot
# Plot (for timepts = 1000) how metric tracks with gramian / centralit of omitted node


# We should merge, subtract, then average
omitvar_tmp <- bringmann_2_omitvar_out$main %>%
  select(ordascont, ord_cls, num_timepts, omit_var, mod_i, est_correct_max_node_omitvar)

nonomit_tmp <- bringmann_2_nonomit_out$main %>%
  select(ordascont, ord_cls, num_timepts, mod_i, est_correct_max_node)

both_merge_tmp <- merge(omitvar_tmp, nonomit_tmp,
                        by = c("ordascont", "ord_cls", "num_timepts", "mod_i")) %>%
  mutate(diff_metric = est_correct_max_node_omitvar - est_correct_max_node)

# Average now across models
mean_models_tmp <- both_merge_tmp %>%
  group_by(ordascont, ord_cls, num_timepts, omit_var) %>%
  summarize(mean_diff_metric = mean(diff_metric),
            se_diff_metric = sd(diff_metric) / sqrt(n()),
            .groups = "drop") %>%
  # mutate(mean_diff_metric_pos = pmax(0, mean_diff_metric_pos)) %>%
  # mutate(mean_diff_metric_neg = -1 * pmax(0, -1 * mean_diff_metric_pos))
  mutate(omit_var = factor(omit_var, ordered = TRUE,
                           levels = paste0("V", seq(1,10))))

## !!! WARNING: this is for ordascont == FALSE for now ##


plot_radar <- function(data_to_plot, column_to_plot, ordascont_val = FALSE) {

  # Subset data
  data_to_plot <- data_to_plot %>% filter(ordascont == ordascont_val)

  # Generate Plot
  ggplot(data_to_plot, aes(x = omit_var)) +
    geom_col(aes(y = !!sym(column_to_plot),
                 fill = !!sym(column_to_plot))) +
    coord_polar() +
    scale_fill_gradient2(low = "darkblue", high = "tomato3",
                         limits = c(-1,1), name = "Metric") +
    # scale_x_discrete(labels = c("Angry",
    #                             "Excited",
    #                             "Happy",
    #                             "Satisfied",
    #                             "Relaxed",
    #                             "Dysphoric",
    #                             "Sad",
    #                             "Anxious",
    #                             "Irritated",
    #                             "Stressed")) +
    scale_y_continuous(
      limits = c(-1,0.5)
    ) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      # Use gray text for the region names
      axis.text.x = element_text(color = "gray12", size = 4.5),

      text = element_text(color = "gray12", family = "Bell MT"),
      plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
      plot.subtitle = element_text(size = 14, hjust = 0.05),
      plot.caption = element_text(size = 10, hjust = .5),

      # Make the background white and remove extra grid lines
      # panel.background = element_blank(),
      # panel.grid = element_line(color = "gray65", linewidth = 0.1),
      # plot.background = element_blank(),
      legend.position = "right"
    ) +
    facet_grid(ord_cls ~ num_timepts)

}




plot_radar(mean_models_tmp, "mean_diff_metric")





# # Average across models
# omit_var_mean_models <- bringmann_2_omitvar_out$main %>%
#   group_by(ordascont, ord_cls, num_timepts, omit_var) %>%
#   summarize(mean_est_max_node_omit = mean(est_correct_max_node_omitvar),
#             se_est_max_node_omit = sd(est_correct_max_node_omitvar) / sqrt(n()),
#             .groups = "drop")
#
# nonomit_mean_models <- bringmann_2_nonomit_out$main %>%
#   group_by(ordascont, ord_cls, num_timepts) %>%
#   summarize(mean_est_max_node_nonomit = mean(est_correct_max_node), .groups = "drop")
#
# test_merge <- merge(omit_var_mean_models, nonomit_mean_models,
#       by = c("ordascont", "ord_cls", "num_timepts"))

#
# test_p <- bringmann_2_omitvar_out$main %>% filter(ordascont == FALSE & mod_i == "model_1" & ord_cls == "ord5" & num_timepts == 1000)
# test_p_non <- bringmann_2_nonomit_out$main %>% filter(ordascont == FALSE & mod_i == "model_1" & ord_cls == "ord5" & num_timepts == 1000)
# test_p_non$est_correct_max_node

# test_p$est_correct_max_node_nonomit <- test_p_non$est_correct_max_node
# test_p <- test_p %>%
#   mutate(est_correct_max_node_omitvar_diff_nonomit = est_correct_max_node_omitvar - est_correct_max_node_nonomit) %>%
#   mutate(est_correct_max_node_omitvar_diff_nonomit_pos = pmax(0, est_correct_max_node_omitvar_diff_nonomit)) %>%
#   mutate(est_correct_max_node_omitvar_diff_nonomit_neg = -1 * pmax(0, -1 * est_correct_max_node_omitvar_diff_nonomit)) %>%
#   mutate(omit_var = factor(omit_var, ordered = TRUE,
#                            levels = paste0("V", seq(1,10))))
#
#
# ggplot(test_p, aes(x = omit_var)) +
#   geom_col(aes(y = est_correct_max_node_omitvar_diff_nonomit_pos,
#                fill = est_correct_max_node_omitvar_diff_nonomit_pos)) +
#   geom_col(aes(y = est_correct_max_node_omitvar_diff_nonomit_neg,
#                fill = est_correct_max_node_omitvar_diff_nonomit_neg)) +
#   coord_polar() +
#   # guides(fill=guide_legend(title="New Legend Title")) +
#   scale_fill_gradient2(low = "darkblue", high = "tomato3",
#                        limits = c(-1,1), name = "TEST") +
#   scale_x_discrete(labels = c("Angry",
#                               "Excited",
#                               "Happy",
#                               "Satisfied",
#                               "Relaxed",
#                               "Dysphoric",
#                               "Sad",
#                               "Anxious",
#                               "Irritated",
#                               "Stressed")) +
#   scale_y_continuous(
#     limits = c(-1,0.5)
#   ) +
#   # theme_bw() +
#   theme(
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     axis.text.y = element_blank(),
#     # Use gray text for the region names
#     axis.text.x = element_text(color = "gray12", size = 8),
#
#     text = element_text(color = "gray12", family = "Bell MT"),
#     plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
#     plot.subtitle = element_text(size = 14, hjust = 0.05),
#     plot.caption = element_text(size = 10, hjust = .5),
#
#     # Make the background white and remove extra grid lines
#     panel.background = element_blank(), # element_rect(fill = "white", color = "white"),
#     panel.grid = element_line(color = "gray65", linewidth = 0.1),
#     # panel.grid.major.x = element_blank(),
#     plot.background = element_blank(),
#     legend.position = "right"
#   )
