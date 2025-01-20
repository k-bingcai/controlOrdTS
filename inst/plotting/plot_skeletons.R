### Code for plotting skeletons for thesis ###
library(devtools)
load_all()

# Set seed
set.seed(1000)

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
