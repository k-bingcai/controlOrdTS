library(devtools)
load_all()

set.seed(1000)

# Two broken chains
broken_chain_template <- two_broken_chains(abs_cst = 2)
qgraph::qgraph(t(broken_chain_template))

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


run_and_plot_simulation(m_func = two_broken_chains,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation")

run_and_plot_simulation(m_func = two_broken_chains,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation")

# 5-node bringmann

bringmann_data1 <- bringmann_2017_dataset1(abs_cst = 2)
qgraph::qgraph(t(bringmann_data1))

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
                        plot_type = "correlation")

run_and_plot_simulation(m_func = bringmann_2017_dataset1,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation")

# 10-node bringmann

bringmann_data2 <- bringmann_2017_dataset2(abs_cst = 2)
qgraph::qgraph(t(bringmann_data2))

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
                        plot_type = "correlation")

run_and_plot_simulation(m_func = bringmann_2017_dataset2,
                        m_func_args = list(abs_cst = 2),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation")


# 10-node scale free
scale_free_mat <- scale_free_seeded(abs_cst = 2, prop_zero = 0.7, seed = 5000)
qgraph::qgraph(t(scale_free_mat))


run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 5000),
                        centrality_type = "degree",
                        metric_type = "dgramian",
                        plot_type = "boxplot")

run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 5000),
                        centrality_type = "degree",
                        metric_type = "max_node",
                        plot_type = "histogram")

run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 5000),
                        centrality_type = "betweenness",
                        metric_type = "dgramian",
                        plot_type = "correlation")

run_and_plot_simulation(m_func = scale_free_seeded,
                        m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = 5000),
                        centrality_type = "closeness",
                        metric_type = "dgramian",
                        plot_type = "correlation")



