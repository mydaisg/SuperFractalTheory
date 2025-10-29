# @author: my@daisg.com
# @date: 2025-10-29
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# L-系统分形(L System)
# L-系统实现
l_system <- function(axiom, rules, depth) {
  current <- axiom
  
  for (i in 1:depth) {
    next_string <- ""
    for (char in strsplit(current, "")[[1]]) {
      if (char %in% names(rules)) {
        next_string <- paste0(next_string, rules[[char]])
      } else {
        next_string <- paste0(next_string, char)
      }
    }
    current <- next_string
  }
  
  return(current)
}

# 绘制科赫曲线
draw_koch <- function(instructions, angle=60, length=10) {
  x <- 0; y <- 0
  direction <- 0
  stack <- list()
  
  coords <- matrix(0, nrow=10000, ncol=2)
  idx <- 1
  coords[idx,] <- c(x, y)
  
  for (cmd in strsplit(instructions, "")[[1]]) {
    if (cmd == "F") {
      # 向前移动
      x <- x + length * cos(direction * pi / 180)
      y <- y + length * sin(direction * pi / 180)
      idx <- idx + 1
      coords[idx,] <- c(x, y)
    } else if (cmd == "+") {
      # 左转
      direction <- direction + angle
    } else if (cmd == "-") {
      # 右转
      direction <- direction - angle
    } else if (cmd == "[") {
      # 保存状态
      stack <- c(list(c(x, y, direction)), stack)
    } else if (cmd == "]") {
      # 恢复状态
      if (length(stack) > 0) {
        state <- stack[[1]]
        stack <- stack[-1]
        x <- state[1]; y <- state[2]; direction <- state[3]
        idx <- idx + 1
        coords[idx,] <- c(x, y)
      }
    }
  }
  
  return(coords[1:idx,])
}

# 生成科赫曲线
koch_rules <- list("F" = "F+F--F+F")
koch_axiom <- "F"
koch_string <- l_system(koch_axiom, koch_rules, 4)
koch_coords <- draw_koch(koch_string, angle=60, length=2)

plot(koch_coords, type="l", asp=1, xlab="", ylab="", 
     main="Koch Snowflake", col="blue", lwd=1)