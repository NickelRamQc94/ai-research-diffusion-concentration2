library(ggplot2)
library(dplyr)

# 构造数据
df_bar <- data.frame(
  Country = c("USA", "China", "Iran", "India", "Germany"),
  Share = c(19, 18, 6, 5, 4)
)

# 按 share 排序（从大到小）
df_bar <- df_bar %>%
  arrange(Share)

# 画图
df_bar <- df_bar %>%
  arrange(desc(Share)) %>%
  mutate(rank = row_number())

p_bar <- ggplot(df_bar, aes(x = Share, y = reorder(Country, Share))) +
  
  geom_col(fill = "#74add1", width = 0.6) +
  
  # ===== 前两个（放柱子中间）=====
geom_text(
  data = subset(df_bar, rank <= 2),
  aes(x = Share / 2,
      label = paste0(Share, "%")),
  hjust = 0.5,
  color = "black",
  size = 24
) +
  
  # ===== 其余（放柱子外侧）=====
geom_text(
  data = subset(df_bar, rank > 2),
  aes(label = paste0(Share, "%")),
  hjust = -0.1,
  size = 24
) +
  
  scale_x_continuous(
    limits = c(0, 21),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = "Share (%)",
    y = NULL
  ) +
  
  theme_classic() +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 63),
    axis.text  = element_text(size = 63),
    axis.line = element_line(linewidth = 2),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15, unit = "pt")
  )

print(p_bar)

ggsave("bar_2014.png", p_bar, width = 12, height = 12, dpi = 600)


#==================== 2023 
df_bar <- data.frame(
  Country = c("China","USA","India", "Germany", "UK"),
  Share = c(39, 15, 6, 3, 2)
)

# 按 share 排序（从大到小）
df_bar <- df_bar %>%
  arrange(Share)

# 画图
df_bar <- df_bar %>%
  arrange(desc(Share)) %>%
  mutate(rank = row_number())

p_bar <- ggplot(df_bar, aes(x = Share, y = reorder(Country, Share))) +
  
  geom_col(fill = "#74add1", width = 0.6) +
  
  # ===== 前两个（放柱子中间）=====
geom_text(
  data = subset(df_bar, rank <= 2),
  aes(x = Share / 2,
      label = paste0(Share, "%")),
  hjust = 0.5,
  color = "black",
  size = 24
) +
  
  # ===== 其余（放柱子外侧）=====
geom_text(
  data = subset(df_bar, rank > 2),
  aes(label = paste0(Share, "%")),
  hjust = -0.1,
  size = 24
) +
  
  scale_x_continuous(
    limits = c(0, 40),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = "Share (%)",
    y = NULL
  ) +
  
  theme_classic() +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 63),
    axis.text  = element_text(size = 63),
    axis.line = element_line(linewidth = 2),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15, unit = "pt")
  )

print(p_bar)
ggsave("bar_2024.png", p_bar, width = 12, height = 12, dpi = 600)
