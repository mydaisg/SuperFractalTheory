# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#曼德勃罗集 (Mandelbrot Set)
mandelbrot <- function(xmin=-2, xmax=1, ymin=-1.5, ymax=1.5, 
                       width=600, height=600, maxiter=100) {
  
  # 创建坐标网格
  x <- seq(xmin, xmax, length.out=width)
  y <- seq(ymin, ymax, length.out=height)
  
  # 初始化结果矩阵
  mandel <- matrix(0, nrow=width, ncol=height)
  
  for (i in 1:width) {
    for (j in 1:height) {
      z <- 0
      c <- complex(real=x[i], imaginary=y[j])
      iter <- 0
      
      while (Mod(z) <= 2 && iter < maxiter) {
        z <- z^2 + c
        iter <- iter + 1
      }
      
      mandel[i,j] <- iter
    }
  }
  
  return(mandel)
}

# 绘制曼德勃罗集
mb <- mandelbrot()
image(mb, col = terrain.colors(100), asp=1, 
      xlab="Real", ylab="Imaginary", 
      main="Mandelbrot Set")