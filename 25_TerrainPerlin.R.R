# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 用ambient包生成Perlin噪声地形
if (!require(ambient)) install.packages("ambient")
library(ambient)

generate_perlin_terrain <- function(width = 256, height = 256) {
  # 生成Perlin噪声
  noise <- noise_perlin(c(width, height))
  
  # 添加分形特性（多重八度）
  for (octave in 2:4) {
    freq <- 2^(octave-1)
    amplitude <- 0.5^(octave-1)
    
    octave_noise <- noise_perlin(c(width, height), frequency = freq)
    noise <- noise + octave_noise * amplitude
  }
  
  # 归一化
  noise <- (noise - min(noise)) / (max(noise) - min(noise))
  return(noise)
}

# 测试Perlin噪声地形
perlin_terrain <- generate_perlin_terrain(128, 128)
image(perlin_terrain, col = terrain.colors(100), 
      main = "Perlin Noise Terrain", asp = 1)