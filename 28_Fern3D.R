# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 生成3D蕨类植物
fern_3d <- function(n_points = 50000) {
  points <- matrix(0, nrow = n_points, ncol = 3)
  x <- 0; y <- 0; z <- 0
  
  for (i in 1:n_points) {
    r <- runif(1)
    
    if (r <= 0.01) {
      # 茎
      x <- 0
      y <- 0.16 * y
      z <- 0.16 * z
    } else if (r <= 0.86) {
      # 小叶子（添加一些3D旋转）
      x_new <- 0.85 * x + 0.04 * y
      y_new <- -0.04 * x + 0.85 * y + 1.6
      z_new <- 0.85 * z + 0.04 * runif(1, -0.1, 0.1)  # 添加一些z轴变化
      x <- x_new; y <- y_new; z <- z_new
    } else if (r <= 0.93) {
      # 左边大叶子
      x_new <- 0.2 * x - 0.26 * y
      y_new <- 0.23 * x + 0.22 * y + 1.6
      z_new <- 0.2 * z + 0.1 * runif(1, -0.2, 0.2)
      x <- x_new; y <- y_new; z <- z_new
    } else {
      # 右边大叶子
      x_new <- -0.15 * x + 0.28 * y
      y_new <- 0.26 * x + 0.24 * y + 0.44
      z_new <- -0.15 * z + 0.1 * runif(1, -0.2, 0.2)
      x <- x_new; y <- y_new; z <- z_new
    }
    
    points[i, ] <- c(x, y, z)
  }
  
  return(points)
}

# 绘制3D蕨类
draw_3d_ferns <- function() {
  library(rgl)
  
  # 生成3D蕨类
  fern_points <- fern_3d(30000)
  
  # 创建颜色渐变（根据高度）
  colors <- colorRampPalette(c("darkgreen", "green", "lightgreen"))(100)
  y_range <- range(fern_points[,2])
  color_indices <- cut(fern_points[,2], breaks = 100, labels = FALSE)
  point_colors <- colors[color_indices]
  
  # 绘制3D场景
  open3d()
  plot3d(fern_points, col = point_colors, size = 1, type = "p",
         xlab = "X", ylab = "Y", zlab = "Z", 
         main = "3D Fractal Fern")
  
  # 添加多个3D蕨类（不同位置和角度）
  rotations <- list(
    matrix(c(1,0,0,0,1,0,0,0,1), nrow=3),  # 无旋转
    matrix(c(0.7,0,0.7,0,1,0,-0.7,0,0.7), nrow=3),  # 绕Y轴旋转45度
    matrix(c(1,0,0,0,0.7,0.7,0,-0.7,0.7), nrow=3)   # 绕X轴旋转45度
  )
  
  positions <- list(c(3,0,0), c(-3,0,0), c(0,0,3))
  
  for (i in 1:length(rotations)) {
    rotated_points <- t(rotations[[i]] %*% t(fern_points))
    translated_points <- t(apply(rotated_points, 1, function(p) p + positions[[i]]))
    
    color_indices <- cut(translated_points[,2], breaks = 100, labels = FALSE)
    point_colors <- colors[color_indices]
    
    points3d(translated_points, col = point_colors, size = 1)
  }
}

# 注意：运行这个函数需要rgl包，并且会打开3D窗口
# draw_3d_ferns()