# ----- Code to simulate gramian vs centrality for given skeleton graphs ----- #

#' Creates empirical graph of Bringmann's paper


#' Creates two broken chains with random parameters
#'
two_broken_chains <- function(abs_cst = 1e-2, eig_cst = 0.5) {
  # We generate two broken chains
  # A: 1 -> 2 -> 3 -> 4 -> 5
  # B: 6 -> 7 -> 8 -> 9 -> 10
  # Z stands for autoregressive
  # A and B stands for the different chains
  m <- matrix(c("Z", "0", "0", "0", "0", "0", "0", "0", "0", "0",
                "A", "Z", "0", "0", "0", "0", "0", "0", "0", "0",
                "0", "A", "Z", "0", "0", "0", "0", "0", "0", "0",
                "0", "0", "A", "Z", "0", "0", "0", "0", "0", "0",
                "0", "0", "0", "A", "Z", "0", "0", "0", "0", "0",
                "0", "0", "0", "0", "0", "Z", "0", "0", "0", "0",
                "0", "0", "0", "0", "0", "B", "Z", "0", "0", "0",
                "0", "0", "0", "0", "0", "0", "B", "Z", "0", "0",
                "0", "0", "0", "0", "0", "0", "0", "B", "Z", "0",
                "0", "0", "0", "0", "0", "0", "0", "0", "B", "Z"), nrow = 10, byrow = TRUE)

  # Get indices
  nonzero_idx           <- which(m != "0")
  AB_idx                <- which(m == "A" | m == "B")
  Z_idx                 <- which(m == "Z")

  # Generate random matrix
  rand_m                <- m
  rand_weights          <- rnorm(n = length(AB_idx))
  rand_m[AB_idx]        <- ifelse(abs(rand_weights) < abs_cst,
                                  sign(rand_weights) * abs_cst,
                                  rand_weights)
  rand_m[Z_idx]         <- rgamma(n = length(Z_idx), shape = 1, rate = 1)
  class(rand_m)         <- "numeric"
  max_eigen             <- max(Mod(eigen(rand_m)$values))
  rand_m                <- rand_m / (max_eigen + eig_cst)

  # Return random matrix
  # This is the transpose of an adjacency matrix with (i,j) means i -> j
  return(rand_m)
}



#' Rank order the nodes based on centrality
#'
rank_nodes_centrality <- function(input_adj_m,
                                  type = c("degree", "betweenness", "closeness")) {

  # Convert adjacency matrix into igraph
  # We run the following preprocessing
  # 1) remove self-connections
  # 2) take the absolute of the reciprocal of the weights
  tf_adj_m                  <- input_adj_m
  diag(tf_adj_m)            <- 0
  tf_adj_m                  <- abs(1 / tf_adj_m)
  tf_adj_m[tf_adj_m == Inf] <- 0
  graph <- igraph::graph_from_adjacency_matrix(tf_adj_m,
                                               mode = "directed",
                                               weighted = TRUE)

  # Compute centrality
  type <- match.arg(type)
  if (type == "degree") {
    centrality <- igraph::degree(graph)
  } else if (type == "betweenness") {
    centrality <- igraph::betweenness(graph)
  } else if (type == "closeness") {
    centrality <- igraph::closeness(graph, mode = "all")
  } else {
    stop("[ERROR] Unsupported centrality type.")
  }

  # Order the nodes (ascending)
  node_order        <- order(centrality)
  centrality_sorted <- sort(centrality)
  node_breaks       <- tail(centrality_sorted, -1) - head(centrality_sorted, -1) > 0

  return(list(node_order = node_order,
              node_values = centrality_sorted,
              node_breaks = node_breaks))
}



#' Compute gramian for random parameter values
#'
simulate_gramian <- function(m_func,
                             num_iter = 1000,
                             m_func_args = list()) {
  # Initialize results vector
  results  <- rep(NA, num_iter)

  # Loop through iterations
  for (i in 1:num_iter) {

    # Generate random matrix
    gen_m <- do.call("m_func", c(m_func_args))

    # Calculate gramian
    dgramian_m            <- diag(netcontrol::control_gramian(A = gen_m,
                                                              B = diag(dim(gen_m)[1])))
    max_gramian_i         <- which(dgramian_m == max(dgramian_m))

    # Update results
    stopifnot(length(max_gramian_i) == 1)
    results[i]            <- max_gramian_i

  }
  return(results)
}


#' Runs and plots the results from the simulation
#'
run_and_plot_simulation <- function(m_func,
                                    m_func_args = list(),
                                    num_iter = 1000,
                                    centrality_type = NULL) {

  # Call m_func once to rank order the nodes
  # Take transpose to get adjacency matrix
  template_m <- t(do.call("m_func", c(m_func_args)))
  nodes_info <- rank_nodes_centrality(template_m, type = centrality_type)

  # Run simulation
  sim_results <- simulate_gramian(m_func = m_func,
                                  m_func_args = m_func_args,
                                  num_iter = num_iter)

  # Plot results
  results_df <- data.frame(x = sim_results) %>% dplyr::count(x)
  results_plot <- ggplot2::ggplot(results_df,
                                  ggplot2::aes(x = factor(x, levels = nodes_info$node_order),
                                  y = n)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::xlab("Node Index ( -> Increasing Centrality )") +
    ggplot2::ylab("Counts") +
    ggplot2::ggtitle(paste("Distribution of Maximal Node (", num_iter, " iterations)", sep = ""))

  # Add equivalent lines!

  # Output graphs
  out_graphs <- list(count_hist = results_plot,
                     var_plot = qgraph::qgraph(template_m))
}

