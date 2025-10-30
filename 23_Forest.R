# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 森林（Forest） 用分形噪声控制树木分布密度
generate_forest <- function(n_trees = 500, clustering = 0.7) {
  # 使用分形噪声控制树木分布密度
  density_map <- fractal_noise_2d(100, 100, 4, 0.6)
  
  trees <- data.frame()
  for (i in 1:n_trees) {
    # 基于密度图概率性放置树木
    x <- sample(1:100, 1)
    y <- sample(1:100, 1)
    
    placement_prob <- density_map[x, y]^clustering
    if (runif(1) < placement_prob) {
      tree_size <- runif(1, 0.5, 2) * placement_prob
      trees <- rbind(trees, data.frame(x = x, y = y, size = tree_size))
    }
  }
  
  return(trees)
}

# 绘制森林
plot_forest <- function() {
  forest <- generate_forest(800, 0.8)
  
  library(ggplot2)
  ggplot(forest, aes(x = x, y = y, size = size)) +
    geom_point(color = "darkgreen", alpha = 0.6) +
    scale_size_identity() +
    theme_void() +
    theme(panel.background = element_rect(fill = "lightgreen")) +
    ggtitle("Fractal Forest Distribution")
}

plot_forest()