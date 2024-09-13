# ----- Code to simulate gramian vs centrality for given skeleton graphs ----- #

#' Helper function to ensure the weights are at least a certain magnitude
clamp_min_weights <- function(weights, abs_cst) {
  out_weights <- ifelse(abs(weights) < abs_cst,
                        sign(weights) * abs_cst,
                        weights)
  return(out_weights)
}


#' Creates empirical graph of Bringmann's paper
#' Creates a 5-node network based on Bringmann's group networks for Dataset-1
#' doi:10.1177/1073191116645909
bringmann_2017_fivenodes <- function(abs_cst = 1e-2, eig_cst = 0.5) {
  # We remove the 'anger' variable from the network
  # Node mapping is given as:
  # N1: Anxious
  # N2: Sad
  # N3: Dysphoric
  # N4: Happy
  # N5: Relaxed
  # "+" stands for positive relations
  # "-" stands for negative weights
  # Rows represent outputs, columns are inputs
  m <- matrix(c("+", "+", "0", "0", "0",
                "0", "+", "+", "-", "0",
                "+", "+", "+", "-", "0",
                "0", "-", "0", "+", "+",
                "0", "0", "0", "+", "+"), nrow = 5, byrow = TRUE)

  # Get indices
  pos_idx           <- which(m == "+")
  neg_idx           <- which(m == "-")
  zero_idx          <- which(m == "0")

  # Generate random matrix
  rand_m                <- m
  rand_pos_weights      <- clamp_min_weights(rgamma(n = length(pos_idx), shape = 1, rate = 1),
                                             abs_cst = abs_cst)
  rand_neg_weights      <- clamp_min_weights(-1 * rgamma(n = length(neg_idx), shape = 1, rate = 1),
                                             abs_cst = abs_cst)
  rand_m[pos_idx]       <- rand_pos_weights
  rand_m[neg_idx]       <- rand_neg_weights
  class(rand_m)         <- "numeric"
  max_eigen             <- max(Mod(eigen(rand_m)$values))
  rand_m                <- rand_m / (max_eigen + eig_cst)

  # Return random matrix
  # This is the transpose of an adjacency matrix with (i,j) means i -> j
  return(rand_m)
}



#' Creates empirical graph of Bringmann's paper
#' Creates a 10-node network based on Bringmann's group networks for Dataset-2
#' doi:10.1177/1073191116645909
bringmann_2017_tennodes <- function(abs_cst = 1e-2, eig_cst = 0.5) {
  # We retain all variables shown in the paper
  # Node mapping is given as:
  # N1: Angry
  # N2: Excited
  # N3: Happy
  # N4: Satisfied
  # N5: Relaxed
  # N6: Dysphoric
  # N7: Sad
  # N8: Anxious
  # N9: Irritated
  # N10: Stressed
  # Rows represent outputs, columns are inputs
  m <- matrix(c("+", "0", "0", "0", "0", "0", "0", "0", "0", "+",
                "0", "+", "+", "+", "0", "0", "0", "0", "0", "0",
                "0", "+", "+", "+", "+", "0", "0", "0", "0", "0",
                "0", "+", "+", "+", "0", "0", "-", "0", "-", "0",
                "0", "+", "+", "+", "+", "0", "0", "0", "0", "-",
                "0", "0", "0", "0", "0", "+", "+", "0", "0", "0",
                "0", "0", "0", "0", "0", "+", "+", "0", "0", "0",
                "0", "0", "0", "0", "0", "0", "0", "+", "0", "+",
                "0", "0", "0", "0", "0", "0", "0", "0", "+", "+",
                "-", "0", "0", "0", "0", "-", "0", "+", "+", "+"), nrow = 10, byrow = TRUE)

  # ------- WE MIGHT WANT TO WRAP THIS AS A FUNCTION --------- #
  # Get indices
  pos_idx           <- which(m == "+")
  neg_idx           <- which(m == "-")
  zero_idx          <- which(m == "0")

  # Generate random matrix
  rand_m                <- m
  rand_pos_weights      <- rgamma(n = length(pos_idx), shape = 1, rate = 1) + abs_cst
  rand_neg_weights      <- -1 * (rgamma(n = length(neg_idx), shape = 1, rate = 1) + abs_cst)
  rand_m[pos_idx]       <- rand_pos_weights
  rand_m[neg_idx]       <- rand_neg_weights
  class(rand_m)         <- "numeric"
  max_eigen             <- max(Mod(eigen(rand_m)$values))
  rand_m                <- rand_m / (max_eigen + eig_cst)

  # Return random matrix
  # This is the transpose of an adjacency matrix with (i,j) means i -> j
  return(rand_m)

}



#' Creates two broken chains with random parameters
#'
two_broken_chains <- function(abs_cst = 1e-2, eig_cst = 0.5) {
  # We generate two broken chains
  # A: 1 -> 2 -> 3 -> 4 -> 5
  # B: 6 -> 7 -> 8 -> 9 -> 10
  # Z stands for autoregressive
  # A and B stands for the different chains
  # Rows represent outputs, columns are inputs
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
  rand_m[AB_idx]        <- clamp_min_weights(rand_weights, abs_cst = abs_cst)
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
                                  type = c("degree")) {

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
  # } else if (type == "betweenness") {
  #   centrality <- igraph::betweenness(graph)
  # } else if (type == "closeness") {
  #   centrality <- igraph::closeness(graph, mode = "all")
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
                             m_func_args = list(),
                             out_type = c("max_node", "dgramian")) {

  # Store output type
  out_type <- match.arg(out_type)

  # Initialize results vector
  results  <- vector("list", num_iter)

  # Loop through iterations
  for (i in 1:num_iter) {

    # Generate random matrix
    gen_m <- do.call("m_func", c(m_func_args))

    # Calculate gramian
    dgramian_m            <- diag(netcontrol::control_gramian(A = gen_m,
                                                              B = diag(dim(gen_m)[1])))

    # Store results depending on out_type
    if (out_type == "max_node") {

      # Store max_node
      max_gramian_i         <- which(dgramian_m == max(dgramian_m))

      # Update results
      stopifnot(length(max_gramian_i) == 1)
      results[[i]]            <- max_gramian_i

    } else if (out_type == "dgramian") {

      # Store entire diagonal gramian
      results[[i]] <- dgramian_m

    }

  }
  return(results)
}


#' Runs and plots the results from the simulation
#'
run_and_plot_simulation <- function(m_func,
                                    m_func_args = list(),
                                    num_iter = 1000,
                                    centrality_type = c("degree", "betweenness", "closeness"),
                                    metric_type = c("max_node", "dgramian")) {

  # Read in default arguments if not given
  centrality_type <- match.arg(centrality_type)
  metric_type     <- match.arg(metric_type)

  # Call m_func once to rank order the nodes
  # Take transpose to get adjacency matrix
  template_m <- t(do.call("m_func", c(m_func_args)))
  nodes_info <- rank_nodes_centrality(template_m, type = centrality_type)

  # Run simulation
  sim_results <- simulate_gramian(m_func = m_func,
                                  m_func_args = m_func_args,
                                  num_iter = num_iter,
                                  out_type = metric_type)

  # Plot results
  if (metric_type == "max_node") {

    # Maximal node results
    results_df <- data.frame(x = unlist(sim_results)) %>%
      mutate(x = factor(x, levels = nodes_info$node_order)) %>%
      dplyr::count(x, .drop = FALSE)

    # Plot object
    results_plot <- ggplot2::ggplot(results_df,
                                    ggplot2::aes(x = x,
                                                 y = n)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::ylab("Counts") +
      ggplot2::ggtitle(paste("Distribution of Maximal Node (", num_iter, " iterations)", sep = ""))

  } else if (metric_type == "dgramian") {

    # Gramian results
    results_df            <- data.frame(do.call(rbind, sim_results))
    colnames(results_df)  <- seq(1, ncol(results_df), 1)
    results_df            <- results_df %>%
                                tidyr::pivot_longer(cols = everything(),
                                                    names_to = "node",
                                                    values_to = "dgramian")

    # Results plot
    results_plot <- ggplot2::ggplot(results_df,
                                    ggplot2::aes(x = factor(node, levels = nodes_info$node_order),
                                                 y = dgramian)) +
      ggplot2::geom_boxplot(outliers = FALSE) +
      ggplot2::ylab("Gramian Value") +
      ggplot2::ggtitle(paste("Distribution of Diagonal Gramian (", num_iter, " iterations)", sep = ""))

  }

  # Common X label
  results_plot <- results_plot +
    ggplot2::xlab(paste("Node Index ( -> increasing ", centrality_type, " )", sep = ""))

  # Add equivalent lines!
  results_plot <- results_plot +
    ggplot2::geom_vline(xintercept = which(nodes_info$node_breaks) + 0.5,
                        color = "red", lty = 2)

  # Output graphs
  out_graphs <- list(out_plot = results_plot,
                     template_m = template_m)
}


# var_plot = qgraph::qgraph(template_m)

# NOTES:
# We need a correlation by node plot for betweenness and closeness
# We might not want to clamp the nodes, instead consider a shift instead
