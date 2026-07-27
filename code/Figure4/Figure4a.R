library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

df_total <- read_csv("National_Annual_All_Disciplines_Summary.csv",
                     show_col_types = FALSE)

df_sel <- df_total %>%
  filter(Year %in% c(2014, 2022, 2024))

lorenz_year <- df_sel %>%
  group_by(Year) %>%
  arrange(Total_Paper_Count, .by_group = TRUE) %>%
  mutate(
    n = n(),
    x = row_number() / n,
    y = cumsum(Total_Paper_Count) / sum(Total_Paper_Count)
  ) %>%
  reframe(
    x = c(0, x),
    y = c(0, y)
  )

p <- ggplot(lorenz_year, aes(x = x, y = y, color = factor(Year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.5) +
  geom_line(linewidth = 3) +
  scale_color_manual(values = c(
    "2014" = "#74add1",
    "2022" = "#fdae61",
    "2024" = "#d73027"
  )) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = c(0, 0)
  ) +
  labs(
    x = "Cumulative share of countries",
    y = "Cumulative share of publications",
    color = "Year"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    axis.title = element_text(size = 21),
    axis.text  = element_text(size = 22),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 30, b = 10, l = 15)
  )

print(p)

ggsave("lorenz_curves_year.png", p, width = 8.8, height = 5, dpi = 600)


#============ 计算与45°的面积
library(dplyr)

# 计算每个年份的洛伦兹曲线下面积
area_df <- lorenz_year %>%
  group_by(Year) %>%
  arrange(x, .by_group = TRUE) %>%
  mutate(
    dx = x - lag(x),
    avg_y = (y + lag(y)) / 2,
    trapezoid = dx * avg_y
  ) %>%
  summarise(
    Lorenz_area = sum(trapezoid, na.rm = TRUE),
    Gini_from_area = 1 - 2 * Lorenz_area,
    Inequality_area = 0.5 - Lorenz_area,
    .groups = "drop"
  )

print(area_df)