# ----- Code to simulate gramian vs centrality for given skeleton graphs ----- #

#' Helper function to ensure the weights are at least a certain magnitude
clamp_min_weights <- function(weights, abs_cst) {
  out_weights <- ifelse(abs(weights) < abs_cst,
                        sign(weights) * abs_cst,
                        weights)
  return(out_weights)
}


#' Creates empirical graph of Bringmann's paper
#' Creates a 6-node network based on Bringmann's group networks for Dataset-1
#' doi:10.1177/1073191116645909
bringmann_2017_dataset1 <- function(abs_cst = 1e-2, eig_cst = 0.5, drop_index = NULL) {
  # Node mapping is given as:
  # N1: Anxious
  # N2: Sad
  # N3: Dysphoric
  # N4: Happy
  # N5: Relaxed
  # N6: Anger
  # "+" stands for positive relations
  # "-" stands for negative weights
  # Rows represent outputs, columns are inputs
  m <- matrix(c("+", "+", "0", "0", "0", "+",
                "0", "+", "+", "-", "0", "0",
                "+", "+", "+", "-", "0", "+",
                "0", "-", "0", "+", "+", "0",
                "0", "0", "0", "+", "+", "0",
                "0", "+", "0", "0", "0", "+"), nrow = 6, byrow = TRUE)

  # Drop the nodes if specified
  if (!is.null(drop_index)) {
    stopifnot(class(drop_index) == "numeric")
    # Delete rows, then columns
    m <- m[-drop_index,][,-drop_index]
  }

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



#' Creates empirical graph of Bringmann's paper
#' Creates a 10-node network based on Bringmann's group networks for Dataset-2
#' doi:10.1177/1073191116645909
bringmann_2017_dataset2 <- function(abs_cst = 1e-2, eig_cst = 0.5, drop_index = NULL) {
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

  # Drop the nodes if specified
  if (!is.null(drop_index)) {
    stopifnot(class(drop_index) == "numeric")
    # Delete rows, then columns
    m <- m[-drop_index,][,-drop_index]
  }

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


#' Calculate centrality for each node based on the adjacency matrix
#'
adj_m_to_centrality <- function(input_adj_m,
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

  # Compute based on type
  if (type == "degree") {
    centrality <- igraph::degree(graph)
  } else if (type == "betweenness") {
    centrality <- igraph::betweenness(graph)
  } else if (type == "closeness") {
    centrality <- igraph::closeness(graph, mode = "all")
  } else {
    stop("[ERROR] Unsupported centrality type.")
  }

  return(centrality)

}


#' Rank order the nodes based on centrality
#'
rank_nodes_centrality <- function(centrality) {

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
                             c_func = NULL,
                             num_iter = 1000,
                             m_func_args = list(),
                             c_func_args = list(),
                             out_type = c("max_node", "dgramian")) {

  # Store output type
  out_type <- match.arg(out_type)

  # Initialize results vector and centrality vector if no function is passed in
  results       <- vector("list", num_iter)
  centrality_m  <- NULL

  # Loop through iterations
  for (i in 1:num_iter) {

    # Generate random matrix
    gen_m <- do.call("m_func", c(m_func_args))

    # Calculate gramian
    dgramian_m            <- diag(netcontrol::control_gramian(A = gen_m,
                                                              B = diag(dim(gen_m)[1])))

    # Calculate centrality if given
    if (!is.null(c_func)) {
      centrality_m <- do.call("c_func", c(list(input_adj_m = gen_m),
                                          c_func_args))
    }

    # Store results depending on out_type
    if (out_type == "max_node") {

      # Store max_node
      max_gramian_i         <- which(dgramian_m == max(dgramian_m))

      # Update results
      stopifnot(length(max_gramian_i) == 1)
      gramian_out           <-  max_gramian_i

    } else if (out_type == "dgramian") {

      # Store entire diagonal gramian
      gramian_out           <- dgramian_m

    }

    # Store results
    results[[i]] <- list(gramian_out = gramian_out,
                         centrality_out = centrality_m)

  }
  return(results)
}


#' Runs and plots the results from the simulation
#'
run_and_plot_simulation <- function(m_func,
                                    m_func_args = list(),
                                    num_iter = 1000,
                                    centrality_type = c("degree", "betweenness", "closeness"),
                                    metric_type = c("max_node", "dgramian"),
                                    plot_type = c("boxplot", "histogram", "correlation")) {

  # Read in default arguments if not given
  centrality_type <- match.arg(centrality_type)
  metric_type     <- match.arg(metric_type)

  # Run simulation
  sim_results <- simulate_gramian(m_func = m_func,
                                  c_func = adj_m_to_centrality,
                                  m_func_args = m_func_args,
                                  c_func_args = list(type = centrality_type),
                                  num_iter = num_iter,
                                  out_type = metric_type)

  # For different plots (WE MIGHT WANT TO SEPARATE THIS INTO ANOTHER SCRIPT)
  if (plot_type == "boxplot") {

    # Only allow for degree + dgramian
    stopifnot(centrality_type == "degree")
    stopifnot(metric_type == "dgramian")

    # Get unique degrees for the matrix
    uniq_degrees <- Reduce(function(x,y) {if (identical(x,y)) x else FALSE},
                           lapply(sim_results, function(x) {x[["centrality_out"]]}))
    if (any(!uniq_degrees)) {
      stop("[ERROR] Non-unique degrees in graph.")
    }

    # Sort the nodes and summarize info
    nodes_info <- rank_nodes_centrality(uniq_degrees)

    # Extract dgramian results
    dgramian_only         <- lapply(sim_results, function(x) {x[["gramian_out"]]})
    results_df            <- data.frame(do.call(rbind, dgramian_only))
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

    # Common X label
    results_plot <- results_plot +
      ggplot2::xlab(paste("Node Index ( -> increasing ", centrality_type, " )", sep = ""))

    # Add equivalent lines!
    results_plot <- results_plot +
      ggplot2::geom_vline(xintercept = which(nodes_info$node_breaks) + 0.5,
                          color = "red", lty = 2)

  } else if (plot_type == "histogram") {

    # Only allow for degree + max_node
    stopifnot(centrality_type == "degree")
    stopifnot(metric_type == "max_node")

    # Get unique degrees for the matrix
    uniq_degrees <- Reduce(function(x,y) {if (identical(x,y)) x else FALSE},
                           lapply(sim_results, function(x) {x[["centrality_out"]]}))
    if (any(!uniq_degrees)) {
      stop("[ERROR] Non-unique degrees in graph.")
    }

    # Sort the nodes and summarize info
    nodes_info <- rank_nodes_centrality(uniq_degrees)

    # Extract dgramian results
    dgramian_only         <- lapply(sim_results, function(x) {x[["gramian_out"]]})

    # Maximal node results
    results_df <- data.frame(x = unlist(dgramian_only)) %>%
      dplyr::mutate(x = factor(x, levels = nodes_info$node_order)) %>%
      dplyr::count(x, .drop = FALSE)

    # Plot object
    results_plot <- ggplot2::ggplot(results_df,
                                    ggplot2::aes(x = x,
                                                 y = n)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::ylab("Counts") +
      ggplot2::ggtitle(paste("Distribution of Maximal Node (", num_iter, " iterations)", sep = ""))

    # Common X label
    results_plot <- results_plot +
      ggplot2::xlab(paste("Node Index ( -> increasing ", centrality_type, " )", sep = ""))

    # Add equivalent lines!
    results_plot <- results_plot +
      ggplot2::geom_vline(xintercept = which(nodes_info$node_breaks) + 0.5,
                          color = "red", lty = 2)

  } else if (plot_type == "correlation") {

    # Do not allow 'degree' or 'max_node'
    if (centrality_type == "degree") {
      stop("[ERROR] Correlations not supported for 'degree'. Degree is degenerate across iterations.")
    }
    if (metric_type == "max_node") {
      stop("[ERROR] Correlations are not supported for 'max_node'. Maximal node is not defined for all nodes. ")
    }

    # Extract results separately
    dgramian_only         <- lapply(sim_results, function(x) {x[["gramian_out"]]})
    centrality_only       <- lapply(sim_results, function(x) {x[["centrality_out"]]})

    # Create matrices for each type
    dgramian_mat <- do.call(rbind, dgramian_only)
    centrality_mat <- do.call(rbind, centrality_only)

    # Loop through the columns
    stopifnot(ncol(dgramian_mat) == ncol(centrality_mat))
    num_nodes   <- ncol(dgramian_mat)
    node_all_df <- NULL
    for (node_i in 1:num_nodes) {

      # Create dataframe for ggplot2
      node_i_df <- data.frame(centrality = centrality_mat[,node_i],
                              dgramian = dgramian_mat[,node_i],
                              node = as.character(node_i))

      # Combine with existing dataframe
      node_all_df <- rbind(node_all_df, node_i_df)

    }

    # Apply facet_wrap scatter plot
    results_plot <- ggplot2::ggplot(node_all_df,
                                    ggplot2::aes(x = centrality, y = dgramian)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ node, ncol = num_nodes)

  }

  # Output graph
  return(results_plot)

}


# var_plot = qgraph::qgraph(template_m)

# NOTES:
# We need a correlation by node plot for betweenness and closeness
# We might not want to clamp the nodes, instead consider a shift instead



# # Plot results
# if (metric_type == "max_node") {
#
#   # Maximal node results
#   results_df <- data.frame(x = unlist(sim_results)) %>%
#     mutate(x = factor(x, levels = nodes_info$node_order)) %>%
#     dplyr::count(x, .drop = FALSE)
#
#   # Plot object
#   results_plot <- ggplot2::ggplot(results_df,
#                                   ggplot2::aes(x = x,
#                                                y = n)) +
#     ggplot2::geom_bar(stat = "identity", width = 0.5) +
#     ggplot2::ylab("Counts") +
#     ggplot2::ggtitle(paste("Distribution of Maximal Node (", num_iter, " iterations)", sep = ""))
#
# } else if (metric_type == "dgramian") {
#
#   # Gramian results
#   results_df            <- data.frame(do.call(rbind, sim_results))
#   colnames(results_df)  <- seq(1, ncol(results_df), 1)
#   results_df            <- results_df %>%
#     tidyr::pivot_longer(cols = everything(),
#                         names_to = "node",
#                         values_to = "dgramian")
#
#   # Results plot
#   results_plot <- ggplot2::ggplot(results_df,
#                                   ggplot2::aes(x = factor(node, levels = nodes_info$node_order),
#                                                y = dgramian)) +
#     ggplot2::geom_boxplot(outliers = FALSE) +
#     ggplot2::ylab("Gramian Value") +
#     ggplot2::ggtitle(paste("Distribution of Diagonal Gramian (", num_iter, " iterations)", sep = ""))
#
# }
#
# # Common X label
# results_plot <- results_plot +
#   ggplot2::xlab(paste("Node Index ( -> increasing ", centrality_type, " )", sep = ""))
#
# # Add equivalent lines!
# results_plot <- results_plot +
#   ggplot2::geom_vline(xintercept = which(nodes_info$node_breaks) + 0.5,
#                       color = "red", lty = 2)
#
# # Output graphs
# out_graphs <- list(out_plot = results_plot,
#                    template_m = template_m)
