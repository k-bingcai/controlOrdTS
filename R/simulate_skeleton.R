# ----- Code to simulate gramian vs centrality for given skeleton graphs ----- #

#' Need a function to generate a random positive definite Psi matrix
random_Psi <- function(nvars) {

  # Create symmetric positive definite matrix
  pos_cst <- 0.5
  L       <- matrix(rnorm(nvars ** 2), nrow = nvars)
  A       <- L %*% t(L)
  c       <- min(eigen(A)$values)
  Psi     <- A + diag(nvars) * (abs(c) + pos_cst)

  # Sanity check
  stopifnot(min(eigen(Psi)$values) > 0)
  return(Psi)
}

#' Helper function to ensure the weights are at least a certain magnitude
clamp_min_weights <- function(weights, abs_cst) {
  out_weights <- ifelse(abs(weights) < abs_cst,
                        sign(weights) * abs_cst,
                        weights)
  return(out_weights)
}


#' Creates fixed scale-free graph
#' 
#' @export
scale_free_seeded <- function(num_nodes = 10, abs_cst = 1e-2, eig_cst = 0.5,
                              prop_zero = 0.8, prob_pos = 0.5,
                              mode_in = c("acyclic", "random"),
                              seed = 100, generate_skeleton = FALSE) {

  # Fix mode
  mode_in <- match.arg(mode_in)

  # Skeleton only
  if (generate_skeleton) {

    # Fix seed for reproducibility
    set.seed(seed)

    # Sample a random scale-free graph
    num_edges <- (num_nodes ** 2) - num_nodes
    m_adj <- igraph::sample_fitness_pl(num_nodes,
                                       floor(num_edges * (1 - prop_zero)),
                                       exponent.out = 2) %>%
      igraph::as.directed(mode = mode_in) %>%
      igraph::as_adjacency_matrix()

    # Add signs
    m_sign  <- sample(c(-1,1), sum(m_adj), TRUE, prob = c(prob_pos, 1 - prob_pos))
    m_adj[m_adj != 0]   <- m_adj[m_adj != 0] * m_sign

    # Exit condition
    return(as.matrix(m_adj))

  } else {

    # Generate skeleton
    m_adj <- scale_free_seeded(num_nodes = num_nodes,
                               abs_cst = abs_cst,
                               eig_cst = eig_cst,
                               prop_zero = prop_zero,
                               prob_pos = prob_pos,
                               mode_in = mode_in,
                               seed = seed,
                               generate_skeleton = TRUE)

    # Reset the seed
    set.seed(NULL)

  }

  # Randomly set the positive and negative weights
  m_scale <- rgamma(n = sum(m_adj != 0), shape = 1, rate = 1) + abs_cst

  # Update the nonzeros
  m_adj[m_adj != 0]   <- m_adj[m_adj != 0] * m_scale 

  # Add positive diagonals
  m_diag <- rgamma(n = num_nodes, shape = 1, rate = 1) + abs_cst
  rand_m  <- m_adj + diag(m_diag)

  # Rescale for stationarity
  max_eigen             <- max(Mod(eigen(rand_m)$values))
  rand_m                <- rand_m / (max_eigen + eig_cst)

  # This is the transpose of an adjacency matrix
  # typical adjacency matrix: (i,j) means i -> j
  return(as.matrix(rand_m))

}


#' Creates empirical graph of Bringmann's paper
#' Creates a 6-node network based on Bringmann's group networks for Dataset-1
#' doi:10.1177/1073191116645909
#' 
#' @export
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
  # This is the transpose of an adjacency matrix
  # typical adjacency matrix: (i,j) means i -> j
  return(rand_m)
}



#' Creates empirical graph of Bringmann's paper
#' Creates a 10-node network based on Bringmann's group networks for Dataset-2
#' doi:10.1177/1073191116645909
#' 
#' @export
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
  m <- matrix(c("+", "0", "0", "0", "0", "0", "0", "0", "+", "0",
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
  # This is the transpose of an adjacency matrix
  # typical adjacency matrix: (i,j) means i -> j
  return(rand_m)

}



#' Creates two broken chains with random parameters
#' 
#' @export
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
  rand_weights_signs    <- sample(c(1,-1), length(AB_idx), replace = TRUE)                                                        # rnorm(n = length(AB_idx))
  rand_weights          <- rgamma(n = length(AB_idx), shape = 1, rate = 1) * rand_weights_signs + abs_cst                         # rand_weights + sign(rand_weights) * abs_cst
  rand_m[AB_idx]        <- rand_weights                                                                                           # clamp_min_weights(rand_weights, abs_cst = abs_cst)
  rand_m[Z_idx]         <- rgamma(n = length(Z_idx), shape = 1, rate = 1) + abs_cst
  class(rand_m)         <- "numeric"
  max_eigen             <- max(Mod(eigen(rand_m)$values))
  rand_m                <- rand_m / (max_eigen + eig_cst)

  # Return random matrix
  # This is the transpose of an adjacency matrix
  # typical adjacency matrix: (i,j) means i -> j
  return(rand_m)
}


#' Calculate centrality for each node based on the adjacency matrix
#'
#' @export
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

    # Sanity check that skeleton is the same
    if (i == 1) {
      skeleton_m <- as.matrix(gen_m) != 0
    } else {
      skeleton_i <- as.matrix(gen_m) != 0
      stopifnot(all(skeleton_m == skeleton_i))
    }

    # Calculate gramian
    dgramian_m            <- diag(netcontrol::control_gramian(A = gen_m,
                                                              B = diag(dim(gen_m)[1])))

    # Calculate centrality if given
    if (!is.null(c_func)) {
      # Use transpose as input!
      centrality_m <- do.call("c_func", c(list(input_adj_m = t(gen_m)),
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



#' Runs and plots simulation from across different scale_free structures 
#' 
#' @export 
sim_multiple_scale_free <- function(num_iter = 1000,
                                    num_seeds = 100) {

  # Create dataframe to store values 
  total_rows <- 3 * num_seeds
  all_results <- data.frame(c_type = rep(NA_character_, total_rows),
                            seed_val = rep(NA_integer_, total_rows),
                            prop_max_node_agree = rep(NA_real_, total_rows))

  # Index for results
  res_index <- 1

  ## Insert for loop here 
  for (seed_k in 1:num_seeds) {

    # Start at 3500 and increase by 100 each time
    cat(paste("Simulating for seed: ", seed_k, "\n"))
    seed_to_use <- 3500 + (seed_k - 1) * 100

    ## Loop through centrality type here 
    for (c_type in c("degree", "betweenness", "closeness")) {

      # Run simulation 
      sim_results <- simulate_gramian(m_func = scale_free_seeded,
                                      c_func = adj_m_to_centrality,
                                      m_func_args = list(abs_cst = 2, prop_zero = 0.7, seed = seed_to_use),
                                      c_func_args = list(type = c_type),
                                      num_iter = num_iter,
                                      out_type = "max_node")
      
      # Function to extract nodes with max centrality
      get_max_centrality <- function(centrality_vec) {
        
        # Get list containing ranking information 
        ranking_info <- rank_nodes_centrality(centrality_vec)

        # Check if there are any maximums
        if (all(!ranking_info$node_breaks)) {
          out_node <- NA 
        } else {
          # Get last TRUE value and extract the equivalent nodes 
          last_break_index <- tail(which(ranking_info$node_breaks), 1)
          out_node <- tail(ranking_info$node_order, -1 * last_break_index)
        }

        return(out_node)
      }

      # Check how many max nodes agree 
      max_node_agree <- lapply(sim_results,
                              function(x) { max_cent_nodes <- get_max_centrality(x[["centrality_out"]]);
                                            ifelse(any(is.na(max_cent_nodes)), NA, any(max_cent_nodes %in% x[["gramian_out"]])) })
      prop_max_node_agree <- mean(unlist(max_node_agree), na.rm = TRUE) 

      # Store results 
      all_results[res_index, "c_type"]                <- c_type
      all_results[res_index, "seed_val"]              <- seed_to_use 
      all_results[res_index, "prop_max_node_agree"]   <- prop_max_node_agree

      # Update index 
      res_index <- res_index + 1

    }

  }

  return(all_results)

}


#' Runs and plots the results from the simulation
#'
#' @export
run_and_plot_simulation <- function(m_func,
                                    m_func_args = list(),
                                    num_iter = 1000,
                                    centrality_type = c("degree", "betweenness", "closeness"),
                                    metric_type = c("max_node", "dgramian"),
                                    plot_type = c("boxplot", "histogram", "correlation_raw", "correlation_summary"),
                                    force_boxplot = FALSE) {

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

    if (!force_boxplot) {

      # Only allow for degree + dgramian
      stopifnot(centrality_type == "degree")
      stopifnot(metric_type == "dgramian")

    } else {

      # Print warning for forcing boxplot
      warning(paste("Forcefully plotting boxplot for metric type", centrality_type))

    }

    # Get unique degrees for the matrix
    uniq_degrees <- Reduce(function(x,y) {if (identical(x,y)) x else FALSE},
                           lapply(sim_results, function(x) {x[["centrality_out"]]}))
    if (is.logical(uniq_degrees)) {
      if (!uniq_degrees) {
        stop("[ERROR] Non-unique degrees in graph.")
      }
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
    results_df            <- results_df %>% dplyr::mutate(node_names = paste("V", node, sep = ""))

    # Results plot
    results_plot <- ggplot2::ggplot(results_df,
                                    ggplot2::aes(x = factor(node_names,
                                                            levels = paste("V", nodes_info$node_order, sep = "")),
                                                 y = dgramian)) +
      ggplot2::geom_boxplot(outliers = FALSE) +
      ggplot2::ylab("Gramian Value") +
      ggplot2::ggtitle(paste("Distribution of Diagonal Gramian")) # (", num_iter, " iterations)", sep = ""))

    # Common X label
    results_plot <- results_plot +
      ggplot2::xlab(paste("Nodes ordered in increasing value of ", 
                          stringr::str_to_title(centrality_type), sep = "")) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8),
                     axis.text.y = ggplot2::element_text(size = 8),
                     axis.title.x = ggplot2::element_text(size = 10),
                     axis.title.y = ggplot2::element_text(size = 10),
                     title = ggplot2::element_text(size = 12)) 
        
        # paste(latex2exp::TeX("$Node Index ( \\rightarrow increasing $"), centrality_type, " )", sep = ""))

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
      dplyr::mutate(node_names = paste("V", x, sep = "")) %>%
      dplyr::mutate(node_names = factor(node_names,
                                        levels = paste("V", nodes_info$node_order, sep = ""))) %>%
      dplyr::count(node_names, .drop = FALSE)
    # results_df <- results_df %>% dplyr::mutate(node_names = paste("V", node, sep = ""))
    # print(head(results_df))
    

    # Plot object
    results_plot <- ggplot2::ggplot(results_df,
                                    ggplot2::aes(x = node_names,
                                                 y = n)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::ylab("Counts") +
      ggplot2::ggtitle(paste("Distribution of Maximal Node")) # (", num_iter, " iterations)", sep = "")) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                axis.title.x = ggplot2::element_text(size = 10),
                axis.title.y = ggplot2::element_text(size = 10),
                title = ggplot2::element_text(size = 12)) 

    # Common X label
    results_plot <- results_plot +
       ggplot2::xlab(paste("Nodes ordered in increasing value of ", 
                          stringr::str_to_title(centrality_type), sep = "")) 

    # Add equivalent lines!
    results_plot <- results_plot +
      ggplot2::geom_vline(xintercept = which(nodes_info$node_breaks) + 0.5,
                          color = "red", lty = 2)

  } else if (plot_type %in% c("correlation_raw", "correlation_summary")) {

    # Do not allow 'degree' or 'max_node'
    if (centrality_type == "degree") {
      stop("[ERROR] Correlations not supported for 'degree'. Degree is degenerate across iterations.")
    }
    if (metric_type == "max_node") {
      stop("[ERROR] Correlations are not supported for 'max_node'. Maximal node is not defined for all nodes. ")
    }
    stopifnot(metric_type == "dgramian")

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
    corr_all_df <- data.frame(corr_val = rep(NA_real_, num_nodes),
                              corr_ci_lower = rep(NA_real_, num_nodes),
                              corr_ci_upper = rep(NA_real_, num_nodes),
                              node_label = rep(NA_character_, num_nodes))
    # Function to get bootstrapped CIs for spearman correlation
    spearman_corr <- function(data, indices) {
      d <- data[indices, ]  # Resample data
      suppressWarnings({
        output <- cor(d[,1], d[,2], method = "spearman")
      })
      return(output)  # Compute Spearman's correlation
    }
    for (node_i in 1:num_nodes) {

      # Create dataframe for ggplot2
      node_i_df <- data.frame(centrality = centrality_mat[,node_i],
                              dgramian = dgramian_mat[,node_i],
                              node = as.character(node_i))

      # print warning
      if (length(unique(node_i_df$centrality)) == 1) {
        warning(paste("Number of unqiue values in centrality is 1."))
      }

      # Create dataframe for correlations 
      suppressWarnings({
        node_i_cor <- cor(node_i_df$centrality,
                          node_i_df$dgramian,
                          method = "spearman")
      })
      node_i_boot_res <- boot::boot(node_i_df[,c("centrality", "dgramian")],
                                    statistic = spearman_corr, R = 1000)
      node_i_boot_ci  <- tryCatch({
        boot::boot.ci(node_i_boot_res, type = "perc")
      }, error = function(e) {
        NA
      }, warning = function(e) {
        NA
      })
      if (!identical(node_i_boot_ci, NA)) {
        boot_ci_lims <- node_i_boot_ci$percent[c(4,5)]
      } else {
        boot_ci_lims <- c(NA, NA)
        warning(paste("Cannot bootstrap CIs for node", node_i))
      }
      corr_all_df[node_i,] <- c(node_i_cor,
                                boot_ci_lims,
                                paste("V",  as.character(node_i), sep = ""))
      
      # print(node_i_df)
      # print(cor(node_i_df$centrality, node_i_df$dgramian, method = "spearman"))

      # Combine with existing dataframe
      node_all_df <- rbind(node_all_df, node_i_df)

    }

    # print("DEBUGGING")
    # return(corr_all_df)
    if (plot_type == "correlation_raw") {


      node_all_df <- node_all_df %>%
        dplyr::mutate(node_names = paste("V", node, sep = "")) %>%
        dplyr::mutate(node_names = factor(node_names, levels = paste("V", seq(1,num_nodes), sep = "")))

      # Apply facet_wrap scatter plot
      results_plot <- ggplot2::ggplot(node_all_df,
                                      ggplot2::aes(x = centrality, y = log(dgramian))) +
        ggplot2::geom_point(alpha = 0.15) +
        ggplot2::facet_wrap(~ node_names, ncol = num_nodes) + 
        ggplot2::theme_bw() + 
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) + 
        ggplot2::xlab(stringr::str_to_title(centrality_type)) + 
        ggplot2::ylab("log(Gramian)") +
        ggplot2::ggtitle(paste("Scatter Plot of Gramian Against ", stringr::str_to_title(centrality_type), sep = "")) + 
        ggplot2::guides(x = ggplot2::guide_axis(angle=45)) 

    } else if (plot_type == "correlation_summary") {

      # Spearman plots
      results_plot <- ggplot2::ggplot(corr_all_df,
            ggplot2::aes(x = factor(node_label,
                        levels = paste("V", seq(1,num_nodes), sep = "")),
                        y = as.numeric(corr_val))) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = as.numeric(corr_ci_lower),
                                          ymax = as.numeric(corr_ci_upper)),
                            linewidth = 0.3,
                            width = 0.3) +
      ggplot2::ylim(c(-1,1)) +
      ggplot2::xlab("Node Index") +
      ggplot2::ylab(latex2exp::TeX("Spearman's $\\rho$")) +
      ggplot2::ggtitle(paste("Correlation Between Gramian and ",
                            stringr::str_to_title(centrality_type), sep = "")) + 
      ggplot2::theme_bw() + 
      ggplot2::geom_hline(yintercept = 0, lty = 2, alpha = 0.3)

    } else {
      stop("Invalid correlation plot type.")
    }

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
