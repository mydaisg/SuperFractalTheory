# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 龙形曲线 (Dragon Curve)
dragon_curve <- function(n) {
  # 初始化方向：右转
  directions <- c(1)  # 1表示右转，-1表示左转
  
  for (i in 1:n) {
    # 每次迭代：复制前一次的方向，中间插入1，然后接上反转的前一次方向
    directions <- c(directions, 1, -rev(directions))
  }
  
  # 将方向转换为坐标
  x <- 0
  y <- 0
  angle <- 0  # 初始角度（向右）
  coords <- matrix(c(x, y), nrow = 1)
  
  for (dir in directions) {
    angle <- angle + dir * pi/2  # 每次旋转90度
    x <- x + cos(angle)
    y <- y + sin(angle)
    coords <- rbind(coords, c(x, y))
  }
  
  return(coords)
}

# 绘制龙形曲线
draw_dragon_curve <- function(iterations = 10) {
  dragon_points <- dragon_curve(iterations)
  plot(dragon_points, type = "l", asp = 1, col = "green4", lwd = 1,
       xlab = "", ylab = "", main = paste("Dragon Curve - Iterations", iterations))
}

par(mfrow = c(2, 2))
for (iter in c(8, 10, 12, 14)) {
  draw_dragon_curve(iter)
}