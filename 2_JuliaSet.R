# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 朱利亚集 (Julia Set)
julia_set <- function(c = complex(real=-0.7, imaginary=0.27),
                      xmin=-1.5, xmax=1.5, ymin=-1.5, ymax=1.5,
                      width=600, height=600, maxiter=100) {
  
  x <- seq(xmin, xmax, length.out=width)
  y <- seq(ymin, ymax, length.out=height)
  julia <- matrix(0, nrow=width, ncol=height)
  
  for (i in 1:width) {
    for (j in 1:height) {
      z <- complex(real=x[i], imaginary=y[j])
      iter <- 0
      
      while (Mod(z) <= 2 && iter < maxiter) {
        z <- z^2 + c
        iter <- iter + 1
      }
      
      julia[i,j] <- iter
    }
  }
  
  return(julia)
}

# 绘制不同参数的朱利亚集
par(mfrow=c(2,2))
for (c_val in c(-0.4+0.6i, -0.7+0.27i, -0.8+0.156i, 0.285+0.01i)) {
  js <- julia_set(c=c_val)
  image(js, col = heat.colors(100), asp=1, main=paste("c ="
        , round(Re(c_val),2), "+", round(Im(c_val),2), "i"))
}