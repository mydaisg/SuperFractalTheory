# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 科赫曲线 (Koch Curve) 迭代法
koch_curve_iterative <- function(n_iter = 4) {
  # 初始线段
  segments <- matrix(c(0, 0, 1, 0), nrow = 2, byrow = TRUE)
  
  for (iter in 1:n_iter) {
    new_segments <- matrix(nrow = 0, ncol = 2)
    
    for (i in 1:(nrow(segments)-1)) {
      start <- segments[i,]
      end <- segments[i+1,]
      
      # 将线段分为四段并添加三角形
      x1 <- start[1]; y1 <- start[2]
      x2 <- end[1]; y2 <- end[2]
      
      dx <- (x2 - x1) / 3
      dy <- (y2 - y1) / 3
      
      p1 <- c(x1, y1)
      p2 <- c(x1 + dx, y1 + dy)
      p4 <- c(x1 + 2*dx, y1 + 2*dy)
      p5 <- c(x2, y2)
      
      # 计算三角形顶点
      angle <- pi/3
      p3_x <- p2[1] + dx * cos(angle) - dy * sin(angle)
      p3_y <- p2[2] + dx * sin(angle) + dy * cos(angle)
      p3 <- c(p3_x, p3_y)
      
      new_segments <- rbind(new_segments, p1, p2, p3, p4)
    }
    new_segments <- rbind(new_segments, p5)
    segments <- new_segments
  }
  
  return(segments)
}

# 测试迭代方法
koch_points <- koch_curve_iterative(4)
plot(koch_points, type = "l", asp = 1, col = "red", lwd = 1,
     main = "Koch Curve - Iterative Method")
