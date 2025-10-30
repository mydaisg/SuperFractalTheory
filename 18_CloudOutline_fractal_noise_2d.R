# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 云彩分形（Cloud Outline）用二维分形噪声生成
# 简单的二维分形噪声生成
fractal_noise_2d <- function(width = 128, height = 128, octaves = 4, persistence = 0.5) {
  # 生成基础噪声
  base_noise <- matrix(runif(width * height), nrow = width, ncol = height)
  result <- matrix(0, nrow = width, ncol = height)
  
  frequency <- 1
  amplitude <- 1
  max_amplitude <- 0
  
  for (i in 1:octaves) {
    # 对当前八度的噪声进行插值
    scaled_noise <- .resize_matrix(base_noise, round(width / frequency), round(height / frequency))
    scaled_noise <- .resize_matrix(scaled_noise, width, height)
    
    result <- result + scaled_noise * amplitude
    max_amplitude <- max_amplitude + amplitude
    
    frequency <- frequency * 2
    amplitude <- amplitude * persistence
  }
  
  # 归一化
  result <- result / max_amplitude
  return(result)
}

# 辅助函数：调整矩阵大小
.resize_matrix <- function(mat, new_width, new_height) {
  old_width <- nrow(mat)
  old_height <- ncol(mat)
  
  x_indices <- seq(1, old_width, length.out = new_width)
  y_indices <- seq(1, old_height, length.out = new_height)
  
  new_mat <- matrix(0, nrow = new_width, ncol = new_height)
  
  for (i in 1:new_width) {
    for (j in 1:new_height) {
      x <- round(x_indices[i])
      y <- round(y_indices[j])
      x <- min(max(x, 1), old_width)
      y <- min(max(y, 1), old_height)
      new_mat[i, j] <- mat[x, y]
    }
  }
  
  return(new_mat)
}

# 生成和显示云彩
generate_cloud_texture <- function() {
  cloud_noise <- fractal_noise_2d(256, 256, 6, 0.6)
  
  # 应用阈值创建云彩形状
  cloud_threshold <- 0.4
  cloud_mask <- ifelse(cloud_noise > cloud_threshold, cloud_noise, 0)
  
  # 绘制
  par(mfrow = c(1, 2))
  image(cloud_noise, col = gray.colors(100), main = "Fractal Noise", asp = 1)
  image(cloud_mask, col = gray.colors(100), main = "Cloud Texture", asp = 1)
}

generate_cloud_texture()