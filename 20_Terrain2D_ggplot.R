# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 地形生成（2D Terrain）使用ggplot2绘制2D地形图
plot_2d_terrain <- function(terrain) {
  library(ggplot2)
  library(reshape2)
  
  # 转换为数据框
  n <- nrow(terrain)
  terrain_df <- melt(terrain)
  names(terrain_df) <- c("x", "y", "height")
  
  ggplot(terrain_df, aes(x = x, y = y, fill = height)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("darkblue", "blue", "lightblue", 
                                    "green", "darkgreen", "brown", "white")) +
    coord_fixed() +
    theme_void() +
    ggtitle("Fractal Terrain - 2D Map") +
    theme(legend.position = "none")
}

plot_2d_terrain(terrain)