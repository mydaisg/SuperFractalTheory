# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 分形维数比较（Fractal Dimensions）
source("./12_CantorSet.R")
source("./7_KochCurve_recursive.R")
source("./10_Sierpinski_Chaos.R")
source("./5_BoxCounting.R")
source("./3_FernIFS.R")
# 计算不同分形的分形维数
compare_fractal_dimensions <- function() {
  fractals <- list(
    "Cantor Set" = function() {
      points <- matrix(nrow = 0, ncol = 2)
      cantor_points <- function(start, end, depth) {
        if (depth == 0) {
          points <<- rbind(points, c(start, 0), c(end, 0))
        } else {
          length <- (end - start) / 3
          cantor_points(start, start + length, depth-1)
          cantor_points(end - length, end, depth-1)
        }
      }
      cantor_points(0, 1, 8)
      return(points)
    },
    
    "Koch Curve" = function() {
      koch_curve_recursive(c(0,0), c(1,0), 6)
    },
    
    "Sierpinski Triangle" = function() {
      sierpinski_chaos(5000)
    }
  )
  
  results <- data.frame()
  for (name in names(fractals)) {
    points <- fractals[[name]]()
    dim_result <- box_counting(points)
    results <- rbind(results, data.frame(
      Fractal = name,
      Dimension = round(dim_result$dimension, 3),
      Points = nrow(points)
    ))
  }
  
  return(results)
}

# 显示分形维数比较
fractal_dims <- compare_fractal_dimensions()
print(fractal_dims)