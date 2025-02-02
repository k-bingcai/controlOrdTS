# Load main library
library(controlOrdTS)
library(ggplot2)

# Seed for replications
set.seed(1000)

# ------------- Two broken chains ---------------- #
run_and_plot_simulation(m_func = two_broken_chains,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "degree",
                        metric_type = "dgramian",
                        plot_type = "boxplot")

run_and_plot_simulation(m_func = two_broken_chains,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "degree",
                        metric_type = "max_node",
                        plot_type = "histogram")

# Use boxplots because betweenness is discrete for broken chains
run_and_plot_simulation(m_func = two_broken_chains,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "boxplot",
                        force_boxplot = TRUE)

# Plot correlations as a function of node index
run_and_plot_simulation(m_func = two_broken_chains,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_summary")


# Plot correlations as a function of node index
run_and_plot_simulation(m_func = two_broken_chains,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_raw")




# --------- Bringmann data 1 ----------- #

run_and_plot_simulation(m_func = bringmann_2017_dataset1,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "degree",
                        metric_type = "dgramian",
                        plot_type = "boxplot")

run_and_plot_simulation(m_func = bringmann_2017_dataset1,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "degree",
                        metric_type = "max_node",
                        plot_type = "histogram")

run_and_plot_simulation(m_func = bringmann_2017_dataset1,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation_summary")

run_and_plot_simulation(m_func = bringmann_2017_dataset1,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation_raw")

run_and_plot_simulation(m_func = bringmann_2017_dataset1,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_summary")

run_and_plot_simulation(m_func = bringmann_2017_dataset1,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_raw")


# ---------- 10 node bringmann ---------- #

run_and_plot_simulation(m_func = bringmann_2017_dataset2,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "degree",
                        metric_type = "dgramian",
                        plot_type = "boxplot")

run_and_plot_simulation(m_func = bringmann_2017_dataset2,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "degree",
                        metric_type = "max_node",
                        plot_type = "histogram")


run_and_plot_simulation(m_func = bringmann_2017_dataset2,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation_summary")

run_and_plot_simulation(m_func = bringmann_2017_dataset2,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation_raw")

run_and_plot_simulation(m_func = bringmann_2017_dataset2,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_summary")

run_and_plot_simulation(m_func = bringmann_2017_dataset2,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_raw")


# ---------- 10 node scale free --------- #
### We first plot for a given fixed structure
run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 3500),
                        centrality_type = "degree",
                        metric_type = "dgramian",
                        plot_type = "boxplot")


run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 3500),
                        centrality_type = "degree",
                        metric_type = "max_node",
                        plot_type = "histogram")

run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 3500),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation_summary")

run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 3500),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation_raw")

run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 3500),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_summary")

run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 3500),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation_raw")

### --- Here we use multiple scale free models --- ###
multi_sf_res <- sim_multiple_scale_free(num_seeds = 100)
ggplot(multi_sf_res,
       aes(x = c_type, y = prop_max_node_agree, fill = c_type)) +
  geom_boxplot(alpha = 0.4) +
  ylim(0,1) +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Centrality Type") +
  ylab("Proportion Agreement") +
  ggtitle("P(Max Gramian Node = Max Centrality Node)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8),
                 axis.text.y = ggplot2::element_text(size = 8),
                 axis.title.x = ggplot2::element_text(size = 10),
                 axis.title.y = ggplot2::element_text(size = 10),
                 title = ggplot2::element_text(size = 10))



# library(ggplot2)
# ggplot(out,
#         aes(x = factor(node_label,
#                        levels = paste("V", 1:10, sep = "")),
#             y = as.numeric(corr_val))) +
#   geom_point() +
#   geom_errorbar(aes(ymin = as.numeric(corr_ci_lower),
#                     ymax = as.numeric(corr_ci_upper)),
#                 linewidth = 0.3,
#                 width = 0.3) +
#   ylim(c(0,1)) +
#   xlab("Node Index") +
#   ylab(latex2exp::TeX("Spearman's $\\rho$")) +
#   ggtitle("Correlation Between Gramian and Centrality") +
#   theme_bw()
#
#
# spearman_corr <- function(data, indices) {
#   d <- data[indices, ]  # Resample data
#   return(cor(d[,1], d[,2], method = "spearman"))  # Compute Spearman's correlation
# }
#
# tmp_data <- data.frame(x = rnorm(30),
#                        y = rnorm(30))
#
# results <- boot::boot(tmp_data, statistic = spearman_corr, R = 1000)
# boot_res <- boot::boot.ci(results, type = "perc")
# Generate plots for correlations

# Run loop for scale-free

# The added packages don't affct the code on longleaf;
# netcontrol is already installed in the conda environment, but in this version we include it as dependent package




