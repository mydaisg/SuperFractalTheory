# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 谢尔宾斯基三角形 (Sierpinski Triangle) 混沌法
sierpinski_chaos <- function(n_points = 10000) {
  # 定义三角形的三个顶点
  vertices <- matrix(c(
    0, 0,
    1, 0,
    0.5, sqrt(3)/2
  ), nrow = 3, byrow = TRUE)
  
  # 随机选择起点
  current_point <- c(runif(1), runif(1))
  points <- matrix(current_point, nrow = 1)
  
  for (i in 1:n_points) {
    # 随机选择一个顶点
    vertex <- vertices[sample(1:3, 1),]
    
    # 移动到当前点和选中顶点的中点
    current_point <- (current_point + vertex) / 2
    points <- rbind(points, current_point)
  }
  
  return(points)
}

# 绘制混沌游戏生成的谢尔宾斯基三角形
sierpinski_points <- sierpinski_chaos(15000)
plot(sierpinski_points, pch = ".", cex = 1, asp = 1, col = "purple",
     xlab = "", ylab = "", main = "Sierpinski Triangle - Chaos Game")
