# @author: my@daisg.com
# @date: 2025-10-30
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
# 蕨类生长动画(Fern Growth Animation)
# 创建蕨类生长动画
fern_growth_animation <- function() {
  library(animation)
  library(ggplot2)
  
  # 设置动画参数
  n_frames <- 50
  points_per_frame <- 1000
  
  # 创建临时目录存储帧
  temp_dir <- tempdir()
  
  # 生成基础蕨类点
  total_points <- n_frames * points_per_frame
  fern_base <- fern_complex(total_points)$points
  
  # 创建生长动画的每一帧
  for (frame in 1:n_frames) {
    # 计算当前帧显示的点数
    current_points <- min(frame * points_per_frame, total_points)
    
    # 创建ggplot图形
    p <- ggplot(data.frame(x = fern_base[1:current_points, 1], 
                           y = fern_base[1:current_points, 2]), 
                aes(x = x, y = y)) +
      geom_point(size = 0.1, color = "darkgreen", alpha = 0.6) +
      coord_fixed() +
      theme_void() +
      theme(panel.background = element_rect(fill = "black")) +
      ggtitle(paste("Fern Growth - Frame", frame))
    
    # 保存帧
    ggsave(paste0(temp_dir, "/fern_frame_", sprintf("%03d", frame), ".png"), 
           p, width = 6, height = 8, dpi = 100)
  }
  
  # 使用ImageMagick创建GIF（如果系统安装了ImageMagick）
  if (Sys.which("magick") != "") {
    system(paste0("magick -delay 10 -loop 0 ", temp_dir, "/fern_frame_*.png ", 
                  temp_dir, "/fern_growth.gif"))
    message("Animation saved as: ", paste0(temp_dir, "/fern_growth.gif"))
  } else {
    message("ImageMagick not found. Frames saved in: ", temp_dir)
  }
}

# 生成动画
# fern_growth_animation()