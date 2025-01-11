### Code for plotting results for prepost

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
bringmann_2_prepost_out <- extract_sim_results(paste0(sim_folder, sim_date),
                                               skeleton = "bringmann_2017_dataset2_prepost",
                                               cols_to_extract = c("post_minus_pre_trace_gramian_rel_bias",
                                                                   "diff_prepost_gramian_sp_rank_corr",
                                                                   "true_post_minus_pre_trace_gramian",
                                                                   "true_prepost_gramian_sp_rank_corr",
                                                                   "warning_bool",
                                                                   "error_bool",
                                                                   "one_cat_only",
                                                                   "num_timepts"))

# Distribution of difference in gramian
bringmann_2_prepost_out$main %>%
  filter(ord_cls == "ordinf" & num_timepts == 1000 & ordascont == FALSE) %>%
  pull(true_post_minus_pre_trace_gramian) %>%
  hist(breaks = 20, main = "Histogram of Post-Pre Trace Gramian")

# Create plot (trace gramian)
plot_boxplots("post_minus_pre_trace_gramian_rel_bias", bringmann_2_prepost_out, log_y = TRUE) +
  geom_hline(yintercept = log(0.1), lty = 3)
plot_boxplots("post_minus_pre_trace_gramian_rel_bias", bringmann_2_prepost_out, log_y = TRUE, ordascont_val = TRUE) +
  geom_hline(yintercept = log(0.1), lty = 3)

# Distribution of prepost correlation
bringmann_2_prepost_out$main %>%
  filter(ord_cls == "ordinf" & num_timepts == 1000 & ordascont == FALSE) %>%
  pull(true_prepost_gramian_sp_rank_corr) %>%
  hist(breaks = 20, main = "Histogram of Correlation between Pre and Post")

# Create plot (diff correlation)
plot_boxplots("diff_prepost_gramian_sp_rank_corr", bringmann_2_prepost_out)
plot_boxplots("diff_prepost_gramian_sp_rank_corr", bringmann_2_prepost_out, ordascont_val = TRUE)
