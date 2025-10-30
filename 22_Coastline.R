# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 分形海岸（Coastline）中点位移法
generate_coastline <- function(n_iter = 10, roughness = 0.7) {
  # 使用中点位移法生成海岸线
  coastline <- midpoint_displacement_1d(n_iter, roughness)
  
  # 添加一些岛屿
  n_points <- length(coastline)
  island_centers <- sample(1:n_points, 3)
  
  for (center in island_centers) {
    island_width <- sample(10:30, 1)
    island_height <- runif(1, 0.1, 0.3)
    
    for (i in max(1, center - island_width):min(n_points, center + island_width)) {
      distance <- abs(i - center) / island_width
      bump <- island_height * cos(distance * pi/2)
      coastline[i] <- coastline[i] + bump
    }
  }
  
  return(coastline)
}

# 绘制海岸线地图
plot_coastline_map <- function() {
  coastline <- generate_coastline(9, 0.6)
  n_points <- length(coastline)
  
  # 创建地图数据
  x <- seq(0, 1, length.out = n_points)
  
  # 创建多边形数据
  map_data <- data.frame(
    x = c(x, rev(x)),
    y = c(coastline, rep(-0.2, n_points))
  )
  
  library(ggplot2)
  ggplot(map_data, aes(x = x, y = y)) +
    geom_polygon(fill = "lightblue", alpha = 0.7) +
    geom_line(data = data.frame(x = x, y = coastline), 
              aes(x = x, y = y), color = "darkblue", size = 1) +
    theme_void() +
    theme(panel.background = element_rect(fill = "blue")) +
    ggtitle("Fractal Coastline")
}

plot_coastline_map()