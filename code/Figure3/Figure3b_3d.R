library(tidyverse)
library(zoo)

df <- read_csv("Netherlands.csv", show_col_types = FALSE)  # 改成你的文件名

df$AI <- as.numeric(df$AI)
df$Non_AI <- as.numeric(df$Non_AI)

# 处理NA值（如果需要）
df <- df %>% drop_na(AI, Non_AI)

# 计算滚动均值和标准差
df2 <- df %>%
  arrange(Year) %>%
  mutate(
    ai_roll_mean = zoo::rollapply(AI, width = 3, FUN = mean, fill = NA, align = "center"),
    ai_roll_sd   = zoo::rollapply(AI, width = 3, FUN = sd,   fill = NA, align = "center"),
    non_ai_roll_mean = zoo::rollapply(Non_AI, width = 3, FUN = mean, fill = NA, align = "center"),
    non_ai_roll_sd   = zoo::rollapply(Non_AI, width = 3, FUN = sd,   fill = NA, align = "center"),
    ymin_ai = ai_roll_mean - ai_roll_sd,
    ymax_ai = ai_roll_mean + ai_roll_sd,
    ymin_non_ai = non_ai_roll_mean - non_ai_roll_sd,
    ymax_non_ai = non_ai_roll_mean + non_ai_roll_sd
  )

# ====== 你自己调这些参数 ======
axis_title_size <- 17
axis_text_size  <- 17

line_width <- 1.8
point_size <- 3.5

shade_year  <- 2018
shade_fill  <- "grey85"  # 阴影颜色更深一点
shade_alpha <- 0.5      # 阴影更“厚”（更不透明）

# ===============================

p <- ggplot(df) +
  annotate("rect",
           xmin = shade_year, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = shade_fill, alpha = shade_alpha) +
  geom_line(aes(x = Year, y = AI), color = "#b2abd2", linewidth = line_width) +
  geom_point(aes(x = Year, y = AI), color = "#b2abd2", size = point_size) +
  geom_line(aes(x = Year, y = Non_AI), color = "#fdb863", linewidth = line_width) +
  geom_point(aes(x = Year, y = Non_AI), color = "#fdb863", size = point_size) +
  scale_x_continuous(
    limits = c(2000, max(df$Year, na.rm = TRUE)),
    breaks = seq(2000, max(df$Year, na.rm = TRUE), by = 5)
  ) +
  scale_y_continuous(
    breaks = seq(
      floor(min(c(df$AI, df$Non_AI), na.rm = TRUE) / 0.5) * 0.5,
      ceiling(max(c(df$AI, df$Non_AI), na.rm = TRUE) / 0.5) * 0.5,
      by = 0.05
    )
  )+
  labs(y = "Gini coefficient") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.title = element_text(size = axis_title_size),
    axis.text  = element_text(size = axis_text_size),
    plot.margin = margin(t = 12, r = 18, b = 12, l = 12, unit = "pt")
  )
print(p)

ggsave("Netherlands.png",
       plot = p, width = 5.5, height = 3.5, dpi = 600)

#==================== Algeria 
df <- read_csv("Algeria.csv", show_col_types = FALSE)  # 改成你的文件名

df$AI <- as.numeric(df$AI)
df$Non_AI <- as.numeric(df$Non_AI)

# 处理NA值（如果需要）
df <- df %>% drop_na(AI, Non_AI)

# 计算滚动均值和标准差
df2 <- df %>%
  arrange(Year) %>%
  mutate(
    ai_roll_mean = zoo::rollapply(AI, width = 3, FUN = mean, fill = NA, align = "center"),
    ai_roll_sd   = zoo::rollapply(AI, width = 3, FUN = sd,   fill = NA, align = "center"),
    non_ai_roll_mean = zoo::rollapply(Non_AI, width = 3, FUN = mean, fill = NA, align = "center"),
    non_ai_roll_sd   = zoo::rollapply(Non_AI, width = 3, FUN = sd,   fill = NA, align = "center"),
    ymin_ai = ai_roll_mean - ai_roll_sd,
    ymax_ai = ai_roll_mean + ai_roll_sd,
    ymin_non_ai = non_ai_roll_mean - non_ai_roll_sd,
    ymax_non_ai = non_ai_roll_mean + non_ai_roll_sd
  )

# ====== 你自己调这些参数 ======
axis_title_size <- 17
axis_text_size  <- 17

line_width <- 1.8
point_size <- 3.5

shade_year  <- 2022
shade_fill  <- "grey85"  # 阴影颜色更深一点
shade_alpha <- 0.5      # 阴影更“厚”（更不透明）

# ===============================

p <- ggplot(df) +
  # 2015 年以后灰色阴影（更明显）
  annotate("rect",
           xmin = shade_year, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = shade_fill, alpha = shade_alpha) +
  # 绘制 AI 和 Non_AI 的折线 + 点
  geom_line(aes(x = Year, y = AI), color = "#b2abd2", linewidth = line_width) +
  geom_point(aes(x = Year, y = AI), color = "#b2abd2", size = point_size) +
  geom_line(aes(x = Year, y = Non_AI), color = "#fdb863", linewidth = line_width) +
  geom_point(aes(x = Year, y = Non_AI), color = "#fdb863", size = point_size) +
  scale_x_continuous(
    breaks = seq(min(df$Year, na.rm = TRUE),
                 max(df$Year, na.rm = TRUE),
                 by = 1)) +
  labs(y = "Gini coefficient") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.title = element_text(size = axis_title_size),
    axis.text  = element_text(size = axis_text_size),
    plot.margin = margin(t = 12, r = 18, b = 12, l = 12, unit = "pt")
  )

print(p)

ggsave("Algeria.png",
       plot = p, width = 5.5, height = 3.5, dpi = 600)



#============================= 添加图例的代码 
library(tidyverse)

df_long <- df %>%
  select(Year, AI, Non_AI) %>%
  pivot_longer(
    cols = c(AI, Non_AI),
    names_to = "Type",
    values_to = "Gini coefficient"
  )

p <- ggplot(df_long) +
  # 2015 年以后灰色阴影
  annotate(
    "rect",
    xmin = shade_year, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    fill = shade_fill, alpha = shade_alpha
  ) +
  
  geom_line(
    aes(x = Year, y = Gini, color = Type),
    linewidth = line_width
  ) +
  geom_point(
    aes(x = Year, y = Gini, color = Type),
    size = point_size
  ) +
  
  scale_color_manual(
    values = c(
      "AI" = "#b2abd2",
      "Non_AI" = "#fdb863"
    ),
    labels = c(
      "AI" = "AI",
      "Non_AI" = "Non-AI"
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(
      min(df$Year, na.rm = TRUE),
      max(df$Year, na.rm = TRUE),
      by = 2
    )
  ) +
  
  labs(
    y = "Gini",
    color = NULL
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.title = element_text(size = axis_title_size),
    axis.text  = element_text(size = axis_text_size),
    legend.position = "right",
    legend.text = element_text(size = axis_text_size),
    plot.margin = margin(t = 12, r = 18, b = 12, l = 12, unit = "pt")
  )

print(p)