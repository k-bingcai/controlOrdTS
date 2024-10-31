# --- Code for plotting --- #

#' Function to extract simulation results
#'
#' @param sim_dir simulation directory
#' @param skeleton name of skeleton to plot for
extract_sim_results <- function(sim_dir,
                                skeleton) {

  # Specify ord classes
  ord_cls <- c("ord3", "ord5", "ord7", "ordinf")

  # Check if dierctories exist
  skeleton_dir <- paste(sim_dir, "models", skeleton, sep = "/")
  stopifnot(file.exists(skeleton_dir))

  # Initialize main df
  main_df <- NULL
  miss_df <- NULL

  # Loop through models in skeleton_dir
  mod_files <- list.files(skeleton_dir)
  for (mod_i in mod_files) {

    # Create string to read file from results
    for (cls in ord_cls) {

      # Specify file paths
      res_i_cls_filename <- paste("TS_list_", mod_i, "_", cls, "_results.RDS",
                                  sep = "")
      res_i_cls_filepath <- paste(skeleton_dir,
                                  mod_i,
                                  "results",
                                  res_i_cls_filename,
                                  sep = "/")

      # Read the results file
      res_i_cls_RDS_raw <- readRDS(res_i_cls_filepath)
      # print(mod_i)
      # print(cls)
      # print(res_i_cls_filepath)

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
     #  print(avg_res)


      # Add back number of timepoints from row names
      avg_res$num_timepts <- as.integer(unlist(lapply(strsplit(rownames(avg_res), split = "=="),
                                                      function(x) x[2])))
      rownames(avg_res) <- NULL

      # Append to main dataframe (mod_i, ord_cls)
      avg_res$mod_i   <- mod_i
      avg_res$ord_cls <- cls
      main_df <- rbind(main_df, avg_res)
      miss_df <- rbind(miss_df, res_i_cls_miss)

    }

  }

  # THIS IS DEBUGGING
  return(list(main = main_df,
              miss = miss_df))

}

z <- extract_sim_results("/home/bing/Desktop/UNC-CH/projects/controlOrdTS_all/resources/simulations/20241023_225706", skeleton = "bringmann_2017_dataset1")
library(ggplot2)
combined_plot <- ggplot(z$main,
                        aes(x = num_timepts, y = est_correct_max_node, color = mod_i)) +
  geom_line(alpha = 0.6,
            position=position_jitter(w=0.00, h=0.05)) +
  # ggtitle(plot_title) +
  facet_grid(~ord_cls) + # ,
             # labeller = labeller(numord = create_ord_plot_label,
             #                     syssize = create_syssize_plot_label)) +
  # ylab(plot_ylab) +
  xlab("Number of Timepoints") +
  scale_x_continuous(limits = c(0, 1000),
                     breaks = c(100, 250, 500, 1000)) +
  theme_bw() +
  theme(legend.position="none")
combined_plot


