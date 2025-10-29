# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 康托尔集 (Cantor Set)
cantor_set <- function(start, end, depth, max_depth, y_level) {
  if (depth > max_depth) {
    return()
  }
  
  # 绘制当前线段
  segments(start, y_level, end, y_level, lwd = (max_depth - depth + 1) * 2)
  
  # 计算新线段的位置
  length <- (end - start) / 3
  new_y <- y_level - 0.1
  
  # 递归处理左右两段
  cantor_set(start, start + length, depth + 1, max_depth, new_y)
  cantor_set(end - length, end, depth + 1, max_depth, new_y)
}

# 绘制康托尔集
draw_cantor_set <- function(depth = 5) {
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       xlab = "", ylab = "", main = paste("Cantor Set - Depth", depth))
  cantor_set(0, 1, 0, depth, 0.9)
}

draw_cantor_set(6)