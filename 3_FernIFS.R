# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 迭代函数系统 (IFS) 蕨类植物分形
fern_ifs <- function(n=10000) {
  points <- matrix(0, nrow=n, ncol=2)
  x <- 0; y <- 0
  
  for (i in 1:n) {
    r <- runif(1)
    
    if (r <= 0.01) {
      # 变换1：茎
      x <- 0
      y <- 0.16 * y
    } else if (r <= 0.86) {
      # 变换2：小叶子
      x <- 0.85 * x + 0.04 * y
      y <- -0.04 * x + 0.85 * y + 1.6
    } else if (r <= 0.93) {
      # 变换3：左边大叶子
      x <- 0.2 * x - 0.26 * y
      y <- 0.23 * x + 0.22 * y + 1.6
    } else {
      # 变换4：右边大叶子
      x <- -0.15 * x + 0.28 * y
      y <- 0.26 * x + 0.24 * y + 0.44
    }
    
    points[i,] <- c(x, y)
  }
  
  return(points)
}

# 绘制蕨类植物
fern_points <- fern_ifs(50000)
plot(fern_points, pch=".", cex=0.5, col="darkgreen", 
     xlab="", ylab="", main="Barnsley Fern", asp=1)