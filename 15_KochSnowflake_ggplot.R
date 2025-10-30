# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 科赫雪花 (Koch Snowflake) 使用ggplot2绘制分形
library(ggplot2)
library(dplyr)
source("./7_KochCurve_recursive.R")
# 使用ggplot2绘制科赫雪花
create_koch_dataframe <- function(depth = 4) {
  points <- koch_curve_recursive(c(0, 0), c(1, 0), depth)
  df1 <- data.frame(x = points[,1], y = points[,2], group = 1)
  
  # 添加其他两条边
  angle <- 2*pi/3
  for (i in 1:2) {
    rotated_points <- t(apply(points, 1, function(p) {
      x <- p[1] - 0.5
      y <- p[2]
      new_x <- x * cos(angle*i) - y * sin(angle*i) + 0.5
      new_y <- x * sin(angle*i) + y * cos(angle*i)
      c(new_x, new_y)
    }))
    df_temp <- data.frame(x = rotated_points[,1], y = rotated_points[,2], group = i+1)
    df1 <- rbind(df1, df_temp)
  }
  
  return(df1)
}

# 绘制ggplot版本
koch_df <- create_koch_dataframe(4)
ggplot(koch_df, aes(x = x, y = y, group = group)) +
  geom_polygon(fill = "lightblue", color = "blue", alpha = 0.7) +
  coord_fixed() +
  theme_void() +
  ggtitle("Koch Snowflake with ggplot2")