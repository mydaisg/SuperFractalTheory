# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 分形树 (Fractal Tree)
fractal_tree <- function(x, y, angle, length, depth, angle_change = pi/6, length_ratio = 0.7) {
  if (depth == 0) return()
  
  # 计算分支终点
  x_end <- x + length * cos(angle)
  y_end <- y + length * sin(angle)
  
  # 绘制分支
  segments(x, y, x_end, y_end, lwd = depth, col = "brown")
  
  # 递归绘制左右分支
  fractal_tree(x_end, y_end, angle - angle_change, length * length_ratio, depth - 1)
  fractal_tree(x_end, y_end, angle + angle_change, length * length_ratio, depth - 1)
}

# 绘制分形树
draw_fractal_tree <- function(depth = 10) {
  plot(0, 0, type = "n", xlim = c(-2, 2), ylim = c(0, 3),
       xlab = "", ylab = "", main = paste("Fractal Tree - Depth", depth))
  fractal_tree(0, 0, pi/2, 0.8, depth)
}

par(mfrow = c(2, 2))
for (depth in c(5, 8, 10, 12)) {
  draw_fractal_tree(depth)
}