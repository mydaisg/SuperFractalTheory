# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 云彩分形（Cloud Outline）用中点位移法生成云彩轮廓
# 一维中点位移法生成云彩轮廓
midpoint_displacement_1d <- function(n_iter = 8, roughness = 0.5) {
  n_points <- 2^n_iter + 1
  height <- numeric(n_points)
  height[1] <- runif(1)
  height[n_points] <- runif(1)
  
  segment_length <- n_points - 1
  displacement_scale <- 1.0
  
  for (i in 1:n_iter) {
    # 处理所有段的中点
    for (j in seq(1, n_points - segment_length, by = segment_length)) {
      mid_point <- j + segment_length / 2
      height[mid_point] <- (height[j] + height[j + segment_length]) / 2 + 
        rnorm(1, 0, displacement_scale * roughness)
    }
    
    segment_length <- segment_length / 2
    displacement_scale <- displacement_scale * (0.5 ^ roughness)
  }
  
  return(height)
}

# 绘制云彩轮廓
draw_cloud_outline <- function() {
  # 生成多个云彩层
  n_layers <- 5
  cloud_data <- data.frame()
  
  for (layer in 1:n_layers) {
    heights <- midpoint_displacement_1d(8, 0.6 + runif(1)*0.3)
    x_vals <- seq(0, 1, length.out = length(heights))
    
    # 添加一些垂直偏移和缩放
    y_offset <- runif(1, 0.2, 0.8)
    scale <- runif(1, 0.3, 0.6)
    
    layer_df <- data.frame(
      x = x_vals,
      y = heights * scale + y_offset,
      layer = layer
    )
    cloud_data <- rbind(cloud_data, layer_df)
  }
  
  # 绘制
  library(ggplot2)
  ggplot(cloud_data, aes(x = x, y = y, group = layer)) +
    geom_polygon(alpha = 0.3, fill = "white", color = NA) +
    theme_void() +
    theme(panel.background = element_rect(fill = "skyblue")) +
    ggtitle("Fractal Cloud Outlines")
}

draw_cloud_outline()