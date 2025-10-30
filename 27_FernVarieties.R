# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 蕨类群（FernVarieties)不同种类的蕨类植物
# 多种蕨类植物的变换参数
fern_varieties <- list(
  "Barnsley" = list(
    transforms = list(
      list(prob = 0.01, a = 0, b = 0, c = 0, d = 0.16, e = 0, f = 0),
      list(prob = 0.85, a = 0.85, b = 0.04, c = -0.04, d = 0.85, e = 0, f = 1.6),
      list(prob = 0.07, a = 0.2, b = -0.26, c = 0.23, d = 0.22, e = 0, f = 1.6),
      list(prob = 0.07, a = -0.15, b = 0.28, c = 0.26, d = 0.24, e = 0, f = 0.44)
    )
  ),
  "Cyrtomium" = list(
    transforms = list(
      list(prob = 0.02, a = 0, b = 0, c = 0, d = 0.25, e = 0, f = -0.4),
      list(prob = 0.84, a = 0.95, b = 0.005, c = -0.005, d = 0.93, e = -0.002, f = 0.5),
      list(prob = 0.07, a = 0.035, b = -0.2, c = 0.16, d = 0.04, e = -0.09, f = 0.02),
      list(prob = 0.07, a = -0.04, b = 0.2, c = 0.16, d = 0.04, e = 0.083, f = 0.12)
    )
  ),
  "Cyathea" = list(
    transforms = list(
      list(prob = 0.01, a = 0, b = 0, c = 0, d = 0.2, e = 0, f = -0.12),
      list(prob = 0.85, a = 0.845, b = 0.035, c = -0.035, d = 0.82, e = 0, f = 1.6),
      list(prob = 0.07, a = 0.2, b = -0.31, c = 0.255, d = 0.245, e = 0, f = 0.29),
      list(prob = 0.07, a = -0.15, b = 0.24, c = 0.25, d = 0.2, e = 0, f = 0.68)
    )
  )
)

# 通用蕨类生成函数
generate_fern_variety <- function(variety_name, n_points = 30000, 
                                  position = c(0, 0), angle = 0, scale = 1) {
  
  if (!variety_name %in% names(fern_varieties)) {
    stop("Unknown fern variety")
  }
  
  variety <- fern_varieties[[variety_name]]
  points <- matrix(0, nrow = n_points, ncol = 2)
  x <- 0; y <- 0
  
  # 计算累积概率
  probs <- sapply(variety$transforms, function(t) t$prob)
  cum_probs <- cumsum(probs)
  
  for (i in 1:n_points) {
    r <- runif(1)
    
    # 选择变换
    transform_idx <- which(r <= cum_probs)[1]
    t <- variety$transforms[[transform_idx]]
    
    # 应用变换
    x_new <- t$a * x + t$b * y + t$e
    y_new <- t$c * x + t$d * y + t$f
    x <- x_new
    y <- y_new
    
    # 旋转和缩放
    theta <- angle * pi / 180
    x_rot <- x * cos(theta) - y * sin(theta)
    y_rot <- x * sin(theta) + y * cos(theta)
    
    points[i, 1] <- position[1] + x_rot * scale
    points[i, 2] <- position[2] + y_rot * scale
  }
  
  return(points)
}

# 绘制多种蕨类
draw_fern_varieties <- function() {
  par(mfrow = c(2, 2))
  
  varieties <- names(fern_varieties)
  colors <- c("darkgreen", "forestgreen", "seagreen")
  
  for (i in 1:length(varieties)) {
    fern_points <- generate_fern_variety(varieties[i], 20000)
    plot(fern_points, pch = ".", cex = 1.2, col = colors[i],
         xlab = "", ylab = "", main = paste(varieties[i], "Fern"),
         xlim = c(-3, 3), ylim = c(0, 10), asp = 1)
  }
  
  # 第四个子图：混合展示
  plot(0, 0, type = "n", xlim = c(-4, 4), ylim = c(0, 10), 
       xlab = "", ylab = "", main = "Mixed Fern Garden", asp = 1)
  
  # 添加多种蕨类
  fern1 <- generate_fern_variety("Barnsley", 15000, c(-2, 2), 10, 0.7)
  fern2 <- generate_fern_variety("Cyrtomium", 15000, c(2, 3), -5, 0.8)
  fern3 <- generate_fern_variety("Cyathea", 15000, c(0, 6), 0, 0.9)
  
  points(fern1, pch = ".", cex = 1, col = "darkgreen")
  points(fern2, pch = ".", cex = 1, col = "forestgreen")
  points(fern3, pch = ".", cex = 1, col = "seagreen")
}

draw_fern_varieties()