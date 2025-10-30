# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 地形生成（3D Terrain）钻石方格地形(diamond square terrain)
diamond_square_terrain <- function(size = 257, roughness = 0.7) {
  # 初始化网格
  n <- size
  terrain <- matrix(0, nrow = n, ncol = n)
  
  # 初始化四个角
  terrain[1, 1] <- runif(1)
  terrain[1, n] <- runif(1)
  terrain[n, 1] <- runif(1)
  terrain[n, n] <- runif(1)
  
  step_size <- n - 1
  random_scale <- 1.0
  
  while (step_size > 1) {
    half_step <- step_size / 2
    
    # 钻石步骤
    for (x in seq(1, n - step_size, by = step_size)) {
      for (y in seq(1, n - step_size, by = step_size)) {
        avg <- mean(c(
          terrain[x, y],
          terrain[x + step_size, y],
          terrain[x, y + step_size],
          terrain[x + step_size, y + step_size]
        ))
        terrain[x + half_step, y + half_step] <- avg + rnorm(1, 0, random_scale)
      }
    }
    
    # 正方形步骤
    for (x in seq(1, n, by = half_step)) {
      start_y <- if (x %% step_size == 1) half_step + 1 else 1
      for (y in seq(start_y, n, by = step_size)) {
        values <- numeric(0)
        
        # 上
        if (x - half_step >= 1) values <- c(values, terrain[x - half_step, y])
        # 下
        if (x + half_step <= n) values <- c(values, terrain[x + half_step, y])
        # 左
        if (y - half_step >= 1) values <- c(values, terrain[x, y - half_step])
        # 右
        if (y + half_step <= n) values <- c(values, terrain[x, y + half_step])
        
        terrain[x, y] <- mean(values) + rnorm(1, 0, random_scale)
      }
    }
    
    step_size <- half_step
    random_scale <- random_scale * (0.5 ^ roughness)
  }
  
  return(terrain)
}

# 绘制3D地形
plot_3d_terrain <- function(terrain) {
  library(rgl)
  
  n <- nrow(terrain)
  x <- 1:n
  y <- 1:n
  
  # 创建颜色映射（根据高度）
  colors <- terrain.colors(100)
  terrain_colors <- colors[cut(terrain, breaks = 100)]
  
  open3d()
  surface3d(x, y, terrain, color = terrain_colors, alpha = 0.8)
  axes3d()
  title3d("Fractal Terrain")
}

# 生成并显示地形
terrain <- diamond_square_terrain(129, 0.6)
plot_3d_terrain(terrain)