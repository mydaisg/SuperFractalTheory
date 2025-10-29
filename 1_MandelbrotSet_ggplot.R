# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 使用ggplot2绘制分形(曼德勃罗集)
library(ggplot2)

# 使用ggplot绘制曼德勃罗集
create_mandelbrot_df <- function(xmin=-2, xmax=1, ymin=-1.5, ymax=1.5, 
                                 resolution=200, maxiter=100) {
  x <- seq(xmin, xmax, length.out=resolution)
  y <- seq(ymin, ymax, length.out=resolution)
  
  df <- expand.grid(x=x, y=y)
  df$iter <- apply(df, 1, function(row) {
    c <- complex(real=row[1], imaginary=row[2])
    z <- 0
    iter <- 0
    while (Mod(z) <= 2 && iter < maxiter) {
      z <- z^2 + c
      iter <- iter + 1
    }
    return(iter)
  })
  
  return(df)
}

mandel_df <- create_mandelbrot_df(resolution=300)
ggplot(mandel_df, aes(x=x, y=y, fill=iter)) +
  geom_tile() +
  scale_fill_viridis_c(option="plasma") +
  coord_fixed() +
  theme_void() +
  ggtitle("Mandelbrot Set (ggplot2 Version)")
