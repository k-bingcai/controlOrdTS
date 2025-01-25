### Code for plotting skeletons for thesis ###
library(devtools)
load_all()

# Set seed
set.seed(1000)

# Broken chain
broken_chain                  <- two_broken_chains(abs_cst = 2)
broken_chain_mat              <- t(broken_chain)
colnames(broken_chain_mat)    <- paste("V", seq(1,10), sep = "")
qgraph::qgraph(broken_chain_mat,
               layout = "circle",
               layoutScale = c(0.8,0.8))

# 6-nodes
bringmann_data1                 <- bringmann_2017_dataset1(abs_cst = 2)
bringmann_data1_mat             <- t(bringmann_data1)
colnames(bringmann_data1_mat)   <- paste("V", seq(1,6), sep = "")
qgraph::qgraph(bringmann_data1_mat,
               nodeNames = c("Anxious",
                             "Sad",
                             "Dysphoric",
                             "Happy",
                             "Relaxed",
                             "Anger"),
               layout = "circle",
               legend.cex = 0.5,
               layoutScale = c(0.8,0.8))


# 10-nodes
bringmann_data2                 <- bringmann_2017_dataset2(abs_cst = 2)
bringmann_data2_mat             <- t(bringmann_data2)
colnames(bringmann_data2_mat)   <- paste("V", seq(1,10), sep = "")
qgraph::qgraph(bringmann_data2_mat,
               nodeNames = c("Angry",
                             "Excited",
                             "Happy",
                             "Satisfied",
                             "Relaxed",
                             "Dysphoric",
                             "Sad",
                             "Anxious",
                             "Irritated",
                             "Stressed"),
               layout = "circle",
               legend.cex = 0.5,
               layoutScale = c(0.8,0.8))


# Plot example scale-free network
# Structure is always the same, but the weights might differ
scale_free <- scale_free_seeded(abs_cst = 2,
                                prop_zero = 0.7,
                                seed = 3500)
scale_free_mat <- t(scale_free)
colnames(scale_free_mat)   <- paste("V", seq(1,10), sep = "")
qgraph::qgraph(scale_free_mat,
               layout = "circle",
               layoutScale = c(0.8,0.8))



# 10-nodes (POST)
bringmann_data2_post <- bringmann_data2
bringmann_data2_post[8,10]  <- 0
bringmann_data2_post[10,8]  <- 0
bringmann_data2_post[9,10]  <- 0
bringmann_data2_post[10,9]  <- 0
bringmann_data2_post[6,7]   <- 0
bringmann_data2_post[7,6]   <- 0
bringmann_data2_post_mat             <- t(bringmann_data2_post)
colnames(bringmann_data2_post_mat)   <- paste("V", seq(1,10), sep = "")
qgraph::qgraph(bringmann_data2_post_mat,
               nodeNames = c("Angry",
                             "Excited",
                             "Happy",
                             "Satisfied",
                             "Relaxed",
                             "Dysphoric",
                             "Sad",
                             "Anxious",
                             "Irritated",
                             "Stressed"),
               layout = "circle",
               legend.cex = 0.5,
               layoutScale = c(0.8,0.8))

