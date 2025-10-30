# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 自然场景（Natural） 综合自然场景
create_natural_scene <- function() {
  library(ggplot2)
  library(patchwork)
  
  # 生成地形
  terrain <- diamond_square_terrain(65, 0.6)
  terrain_df <- melt(terrain)
  names(terrain_df) <- c("x", "y", "height")
  
  # 生成云彩
  cloud_data <- data.frame()
  for (layer in 1:3) {
    heights <- midpoint_displacement_1d(7, 0.5)
    x_vals <- seq(0, 1, length.out = length(heights))
    layer_df <- data.frame(
      x = x_vals,
      y = heights * 0.2 + 0.7 + runif(1)*0.1,
      layer = layer
    )
    cloud_data <- rbind(cloud_data, layer_df)
  }
  
  # 生成树木
  forest <- generate_forest(200, 0.6)
  forest$x <- forest$x / 100  # 归一化
  forest$y <- forest$y / 100
  
  # 绘制综合场景
  p1 <- ggplot(terrain_df, aes(x = x, y = y, fill = height)) +
    geom_tile() +
    scale_fill_gradientn(colors = terrain.colors(100)) +
    coord_fixed() +
    theme_void() +
    ggtitle("Terrain") +
    theme(legend.position = "none")
  
  p2 <- ggplot(cloud_data, aes(x = x, y = y, group = layer)) +
    geom_polygon(alpha = 0.3, fill = "white", color = NA) +
    theme_void() +
    theme(panel.background = element_rect(fill = "skyblue")) +
    ggtitle("Clouds")
  
  p3 <- ggplot(forest, aes(x = x, y = y, size = size)) +
    geom_point(color = "darkgreen", alpha = 0.6) +
    scale_size_identity() +
    theme_void() +
    theme(panel.background = element_rect(fill = "lightgreen")) +
    ggtitle("Forest")
  
  # 组合图形
  (p1 | p2 | p3) + plot_annotation(title = "Natural Fractal Scenes")
}

create_natural_scene()