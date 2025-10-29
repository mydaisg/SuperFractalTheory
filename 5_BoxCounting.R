# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 盒计数法计算分形维数(Box Counting)
# 盒计数法计算分形维数
box_counting <- function(data, box_sizes = 2^(1:8)) {
  counts <- numeric(length(box_sizes))
  
  for (i in 1:length(box_sizes)) {
    box_size <- box_sizes[i]
    
    # 将数据空间划分为盒子
    x_range <- range(data[,1])
    y_range <- range(data[,2])
    
    x_boxes <- seq(x_range[1], x_range[2], length.out = box_size + 1)
    y_boxes <- seq(y_range[1], y_range[2], length.out = box_size + 1)
    
    # 计算包含点的盒子数量
    box_count <- 0
    for (x_idx in 1:box_size) {
      for (y_idx in 1:box_size) {
        in_box <- data[,1] >= x_boxes[x_idx] & data[,1] < x_boxes[x_idx + 1] &
          data[,2] >= y_boxes[y_idx] & data[,2] < y_boxes[y_idx + 1]
        if (any(in_box)) {
          box_count <- box_count + 1
        }
      }
    }
    
    counts[i] <- box_count
  }
  
  # 线性回归计算分形维数
  log_box_sizes <- log(1/box_sizes)
  log_counts <- log(counts)
  
  fit <- lm(log_counts ~ log_box_sizes)
  fractal_dim <- coef(fit)[2]
  
  return(list(dimension = fractal_dim, 
              fit = fit,
              data = data.frame(box_size = box_sizes, count = counts)))
}

# 示例：计算蕨类植物的分形维数
fern_points <- fern_ifs(10000)
result <- box_counting(fern_points)
print(paste("分形维数:", round(result$dimension, 3)))

# 绘制拟合结果
plot(log(1/result$data$box_size), log(result$data$count), 
     xlab = "log(1/box_size)", ylab = "log(N)",
     main = "Box Counting Method")
abline(result$fit, col="red")