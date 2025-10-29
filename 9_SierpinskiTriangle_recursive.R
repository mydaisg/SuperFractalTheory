# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 谢尔宾斯基三角形 (Sierpinski Triangle) 递归法
sierpinski_triangle <- function(vertices, depth) {
  # vertices: 3x2矩阵，包含三角形的三个顶点
  if (depth == 0) {
    # 在深度为0时绘制三角形
    return(vertices)
  } else {
    # 计算三个中点
    a <- vertices[1,]
    b <- vertices[2,]
    c <- vertices[3,]
    
    ab <- (a + b) / 2
    ac <- (a + c) / 2
    bc <- (b + c) / 2
    
    # 递归生成三个小三角形
    triangle1 <- sierpinski_triangle(rbind(a, ab, ac), depth-1)
    triangle2 <- sierpinski_triangle(rbind(b, ab, bc), depth-1)
    triangle3 <- sierpinski_triangle(rbind(c, ac, bc), depth-1)
    
    return(rbind(triangle1, triangle2, triangle3))
  }
}

# 绘制谢尔宾斯基三角形
draw_sierpinski <- function(depth = 5) {
  # 定义初始大三角形
  size <- 1
  height <- size * sqrt(3) / 2
  
  vertices <- matrix(c(
    0, 0,
    size, 0,
    size/2, height
  ), nrow = 3, byrow = TRUE)
  
  # 生成谢尔宾斯基三角形点
  points <- sierpinski_triangle(vertices, depth)
  
  # 绘制
  plot(points, pch = 20, cex = 0.3, asp = 1, col = "darkred",
       xlab = "", ylab = "", 
       main = paste("Sierpinski Triangle - Depth", depth))
}

# 绘制不同深度
par(mfrow = c(2, 3))
for (depth in 0:5) {
  draw_sierpinski(depth)
}