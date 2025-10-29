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


# Error in plot.new() : figure margins too large 报错的处理方式
# 是因为绘图区域相对于图形设备或窗口来说太小了
# 在一个画面中绘制多个图形（比如使用了 par(mfrow=c(2,2))），
# 但分配给Plots窗口的物理尺寸不足以容纳你设定的复杂图形布局和边距时
# 可以通过几种方法来调整：

# 1、 调整RStudio的Plots窗口
# 找到RStudio界面右下角的 Plots 窗口。
# 用鼠标拖拽这个窗口的边框，将其拉大。
# 再次运行你的绘图代码。
# 宽哥采用的是这种方式，在i5CPU和4GB内存环境下，要等2分钟才能显示4图

# 2、 调整图形边距参数
# 通过 par()函数中的 mar参数可以减少每个子图周围的边距，
# 为绘图区域“挤”出更多空间。默认的边距值（c(5, 4, 4, 2) + 0.1）可能偏大
# 可以在设置 mfrow的同时或之前，设置更紧凑的边距：
# 在绘图前设置参数
# par(mfrow = c(2, 2), mar = c(2, 2, 1, 1)) # 下、左、上、右的边距都减小了

# 3、调整图形设备尺寸
# 如果调整边距后图形仍然显得拥挤，可以尝试直接创建一个更大的图形窗口。
# win.graph()函数（或在Linux/Mac下的 x11()）可以指定图形窗口的宽高
# 创建一个足够大的新窗口来容纳4个子图
# win.graph(width=10, height=10) # 宽度和高度均为10英寸
# # 或者使用 x11(width=10, height=10)
# par(mfrow = c(2, 2))
# # ... 绘图代码 ...

# 4、将图形输出到文件
# 将图形直接保存到文件（如PNG或PDF）可以彻底绕过图形窗口尺寸的限制。
# 这对于生成高质量、可打印的图片也非常有用
# 开启PDF图形设备，并指定文件名和尺寸
# pdf("my_julia_sets.pdf", width=10, height=10)
# par(mfrow = c(2, 2), mar = c(2, 2, 1, 1)) # 可以结合使用更紧凑的边距
# # ... 绘图代码 ...
# # 关闭图形设备，保存文件
# dev.off()


