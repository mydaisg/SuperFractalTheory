# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 谢尔宾斯基地毯 (Sierpinski Carpet)
sierpinski_carpet <- function(x, y, size, depth, max_depth) {
  if (depth == max_depth) {
    # 在最大深度绘制黑色正方形
    rect(x, y, x + size, y + size, col = "black", border = NA)
  } else {
    # 将正方形分为9个小正方形
    new_size <- size / 3
    
    for (i in 0:2) {
      for (j in 0:2) {
        if (i == 1 && j == 1) {
          # 中间的正方形留白
          rect(x + new_size, y + new_size, 
               x + 2*new_size, y + 2*new_size, 
               col = "white", border = NA)
        } else {
          # 递归处理其他8个正方形
          sierpinski_carpet(x + i*new_size, y + j*new_size, 
                            new_size, depth + 1, max_depth)
        }
      }
    }
  }
}

# 绘制谢尔宾斯基地毯
draw_sierpinski_carpet <- function(depth = 4) {
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), asp = 1,
       xlab = "", ylab = "", main = paste("Sierpinski Carpet - Depth", depth))
  sierpinski_carpet(0, 0, 1, 0, depth)
}

# 绘制不同深度
par(mfrow = c(2, 3))
for (depth in 1:6) {
  draw_sierpinski_carpet(depth)
}