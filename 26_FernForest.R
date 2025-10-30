# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 蕨类森林（FernForest)
# 增强版的蕨类植物生成器，支持多方向和变换
fern_complex <- function(n_points = 50000, 
                         position = c(0, 0), 
                         angle = 0, 
                         scale = 1,
                         mirror = FALSE,
                         color = "darkgreen") {
  
  points <- matrix(0, nrow = n_points, ncol = 2)
  x <- 0; y <- 0
  
  for (i in 1:n_points) {
    r <- runif(1)
    
    if (r <= 0.01) {
      # 变换1：茎
      x <- 0
      y <- 0.16 * y
    } else if (r <= 0.86) {
      # 变换2：小叶子
      x_new <- 0.85 * x + 0.04 * y
      y_new <- -0.04 * x + 0.85 * y + 1.6
      x <- x_new; y <- y_new
    } else if (r <= 0.93) {
      # 变换3：左边大叶子
      x_new <- 0.2 * x - 0.26 * y
      y_new <- 0.23 * x + 0.22 * y + 1.6
      x <- x_new; y <- y_new
    } else {
      # 变换4：右边大叶子
      x_new <- -0.15 * x + 0.28 * y
      y_new <- 0.26 * x + 0.24 * y + 0.44
      x <- x_new; y <- y_new
    }
    
    # 镜像变换（如果需要）
    if (mirror) {
      x <- -x
    }
    
    # 旋转和缩放
    theta <- angle * pi / 180
    x_rot <- x * cos(theta) - y * sin(theta)
    y_rot <- x * sin(theta) + y * cos(theta)
    
    # 应用缩放和平移
    points[i, 1] <- position[1] + x_rot * scale
    points[i, 2] <- position[2] + y_rot * scale
  }
  
  return(list(points = points, color = color))
}

# 绘制多方向蕨类森林
draw_fern_forest <- function() {
  # 创建画布
  plot(0, 0, type = "n", xlim = c(-5, 5), ylim = c(0, 10), 
       xlab = "", ylab = "", main = "Complex Fern Forest", asp = 1)
  
  # 定义多个蕨类的参数
  ferns <- list(
    list(position = c(0, 2), angle = 0, scale = 0.8, mirror = FALSE, color = "darkgreen"),
    list(position = c(-2, 3), angle = 30, scale = 0.6, mirror = FALSE, color = "forestgreen"),
    list(position = c(2, 3), angle = -30, scale = 0.6, mirror = TRUE, color = "seagreen"),
    list(position = c(-3, 5), angle = 15, scale = 0.5, mirror = FALSE, color = "darkolivegreen"),
    list(position = c(3, 5), angle = -15, scale = 0.5, mirror = TRUE, color = "olivedrab"),
    list(position = c(0, 7), angle = 0, scale = 1.2, mirror = FALSE, color = "darkgreen"),
    list(position = c(-1, 1), angle = 10, scale = 0.4, mirror = FALSE, color = "lightseagreen"),
    list(position = c(1, 1), angle = -10, scale = 0.4, mirror = TRUE, color = "mediumseagreen")
  )
  
  # 绘制所有蕨类
  for (fern_params in ferns) {
    fern_data <- do.call(fern_complex, fern_params)
    points(fern_data$points, pch = ".", cex = 0.8, col = fern_data$color)
  }
  
  # 添加地面
  rect(-5, 0, 5, 0.5, col = "saddlebrown", border = NA)
}

draw_fern_forest()