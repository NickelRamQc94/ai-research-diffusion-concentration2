library(tidyverse)
library(readr)
library(ggplot2)

# 1️⃣ 读取数据
df <- read_csv("left_file_with_income_group.csv", show_col_types = FALSE)

# 2️⃣ 按 Year 和 Income group 汇总 Paper_Count
df_count <- df %>%
  filter(!is.na(Year), !is.na(`Income group`), !is.na(Country_std)) %>%
  distinct(Year, `Income group`, Country_std) %>%
  group_by(Year, `Income group`) %>%
  summarise(Country_Count = n(), .groups = "drop") %>%
  mutate(
    `Income group` = factor(
      `Income group`,
      levels = c(
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      )
    )
  ) %>%
  arrange(Year, `Income group`)

# 3️⃣ 绘图
p <- ggplot() +
  geom_bar(
    data = df_count,
    aes(x = Year, y = Country_Count, fill = `Income group`),
    stat = "identity",
    width = 0.8
  ) +
  scale_y_continuous(
    name = "Number of countries",
    limits = c(0, 120),
    expand = expansion(mult = c(0, 0.0))
  ) +
  scale_x_continuous(
    limits = c(1995, 2024),
    breaks = seq(2000, max(df_count$Year, na.rm = TRUE), by = 5),,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(values = c(
    "Low income" = "#F2C14E",
    "Lower middle income" = "#99D096",
    "Upper middle income" = "#53A8E1",
    "High income" = "#FF8081"
  )) +
  labs(x = "Year", fill = "") +
  theme_classic(base_size = 22) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    legend.position = "top"
  )

print(p)

ggsave("country_share_four_income.png", p, width = 7.5, height = 6, dpi = 600)
