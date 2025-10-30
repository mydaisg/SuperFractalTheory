# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 山脉生成（Mountains）多重八度噪声山脉
generate_mountains <- function(width = 512, height = 512) {
  # 生成基础地形
  base_terrain <- diamond_square_terrain(129, 0.5)
  base_terrain <- .resize_matrix(base_terrain, width, height)
  
  # 添加细节噪声
  detail_noise <- fractal_noise_2d(width, height, 3, 0.8)
  
  # 组合基础地形和细节
  mountains <- base_terrain^1.5 + detail_noise * 0.3
  
  # 应用山峰效果
  center_x <- width / 2
  center_y <- height / 2
  max_dist <- sqrt(center_x^2 + center_y^2)
  
  for (x in 1:width) {
    for (y in 1:height) {
      dist <- sqrt((x - center_x)^2 + (y - center_y)^2)
      # 距离中心越远，高度越低（创建山脉范围）
      distance_factor <- 1 - (dist / max_dist)^2
      mountains[x, y] <- mountains[x, y] * max(0, distance_factor)
    }
  }
  
  return(mountains)
}

# 绘制山脉
mountain_terrain <- generate_mountains(256, 256)
image(mountain_terrain, col = terrain.colors(100), 
      main = "Fractal Mountains", asp = 1)