# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 科赫曲线 (Koch Curve) 递归法
# 递归生成科赫曲线点
koch_curve_recursive <- function(start, end, depth) {
  if (depth == 0) {
    return(matrix(c(start, end), nrow=2, byrow=TRUE))
  } else {
    # 将线段分为四段
    x1 <- start[1]; y1 <- start[2]
    x2 <- end[1]; y2 <- end[2]
    
    # 计算四个关键点
    dx <- (x2 - x1) / 3
    dy <- (y2 - y1) / 3
    
    p1 <- c(x1, y1)
    p2 <- c(x1 + dx, y1 + dy)
    p4 <- c(x1 + 2*dx, y1 + 2*dy)
    p5 <- c(x2, y2)
    
    # 计算等边三角形的顶点（科赫曲线的凸起部分）
    angle <- pi/3  # 60度
    p3_x <- p2[1] + dx * cos(angle) - dy * sin(angle)
    p3_y <- p2[2] + dx * sin(angle) + dy * cos(angle)
    p3 <- c(p3_x, p3_y)
    
    # 递归生成四个线段
    segment1 <- koch_curve_recursive(p1, p2, depth-1)
    segment2 <- koch_curve_recursive(p2, p3, depth-1)
    segment3 <- koch_curve_recursive(p3, p4, depth-1)
    segment4 <- koch_curve_recursive(p4, p5, depth-1)
    
    return(rbind(segment1, segment2[-1,], segment3[-1,], segment4[-1,]))
  }
}

# 绘制科赫雪花
draw_koch_snowflake <- function(depth = 4) {
  # 等边三角形的三个顶点
  size <- 1
  height <- size * sqrt(3) / 2
  
  p1 <- c(0, 0)
  p2 <- c(size, 0)
  p3 <- c(size/2, height)
  
  # 生成三条科赫曲线
  side1 <- koch_curve_recursive(p1, p2, depth)
  side2 <- koch_curve_recursive(p2, p3, depth)
  side3 <- koch_curve_recursive(p3, p1, depth)
  
  # 合并所有点
  snowflake <- rbind(side1, side2[-1,], side3[-1,])
  
  # 绘制
  plot(snowflake, type = "l", asp = 1, col = "blue", lwd = 1,
       xlab = "", ylab = "", main = paste("Koch Snowflake - Depth", depth))
  polygon(snowflake, col = rgb(0.7, 0.8, 1, 0.3), border = "blue")
}

# 绘制不同深度的科赫雪花
par(mfrow = c(2, 3))
for (depth in 0:5) {
  draw_koch_snowflake(depth)
}