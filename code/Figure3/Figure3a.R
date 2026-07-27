#==================== 加权的方法计算  ===============================
#==================== 加权的方法计算  ===============================
#==================== 加权的方法计算  ===============================

library(readr)
library(dplyr)
library(ggplot2)

#========================
# 1 读入数据
#========================

pub_df <- read_csv("Country_Year_PaperCount_Sum.csv", show_col_types = FALSE)
gini_df <- read_csv("left_file_with_income_group.csv", show_col_types = FALSE)

#========================
# 2 整理列格式
#========================

pub_df <- pub_df %>%
  mutate(
    Country = trimws(Country),
    Year = as.numeric(Year),
    Count = as.numeric(Count)
  )

gini_df <- gini_df %>%
  mutate(
    Country = trimws(Country),
    Year = as.numeric(Year),
    gini_pub = as.numeric(gini_pub),
    `Income group` = trimws(`Income group`)
  )

#========================
# 3 按 Country + Year 合并 Gini 和发文量
#========================

df_weight <- gini_df %>%
  inner_join(pub_df, by = c("Country", "Year")) %>%
  filter(
    !is.na(gini_pub),
    !is.na(Count),
    Count > 0
  )

# 检查合并后的数据
print(head(df_weight))
print(nrow(df_weight))

#========================
# 4 Global 加权 Gini
#========================

g_global_w <- df_weight %>%
  group_by(Year) %>%
  summarise(
    weighted_gini = sum(gini_pub * Count, na.rm = TRUE) / sum(Count, na.rm = TRUE),
    total_pub = sum(Count, na.rm = TRUE),
    n_country = n_distinct(Country),
    .groups = "drop"
  ) %>%
  mutate(Group = "Global")

#========================
# 5 Income group 加权 Gini
#========================

g_income_w <- df_weight %>%
  filter(!is.na(`Income group`)) %>%
  filter(`Income group` != "Low income") %>%
  group_by(Year, `Income group`) %>%
  summarise(
    weighted_gini = sum(gini_pub * Count, na.rm = TRUE) / sum(Count, na.rm = TRUE),
    total_pub = sum(Count, na.rm = TRUE),
    n_country = n_distinct(Country),
    .groups = "drop"
  ) %>%
  mutate(Group = `Income group`) %>%
  dplyr::select(-`Income group`)

#========================
# 6 合并结果
#========================

gini_summary_weighted <- bind_rows(g_global_w, g_income_w) %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "Global",
        "Lower middle income",
        "Upper middle income",
        "High income"
      )
    )
  )

# 输出加权结果
print(gini_summary_weighted)
write_csv(gini_summary_weighted, "gini_summary_publication_weighted.csv")
#========================
# 7 画图
#========================
peak_year <- 2014

my_cols <- c(
  "Global"              = "grey80",
  "Lower middle income" = "#99D096",
  "Upper middle income" = "#53A8E1",
  "High income"         = "#FF8081"
)

p_weighted <- ggplot(
  gini_summary_weighted,
  aes(x = Year, y = weighted_gini, color = Group)
) +
  annotate(
    "rect",
    xmin = peak_year, xmax = 2024,
    ymin = -Inf, ymax = Inf,
    fill = "grey85", alpha = 0.4
  ) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(
    breaks = seq(2000, 2024, by = 5),
    limits = c(2000, 2024),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Year",
    y = "Gini coefficient"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.title = element_text(size = 16, color = "black"),
    axis.text  = element_text(size = 14, color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 20)
  )

print(p_weighted)

ggsave(
  "publication_weighted_gini.png",
  plot = p_weighted,
  width = 8.5,
  height = 4.5,
  dpi = 600
)

#======================加入低收入的 ==========================
library(readr)
library(dplyr)
library(ggplot2)

#========================
# 1 读入数据
#========================
pub_df <- read_csv("Country_Year_PaperCount_Sum.csv", show_col_types = FALSE)
gini_df <- read_csv("left_file_with_income_group.csv", show_col_types = FALSE)

#========================
# 2 整理数据
#========================
pub_df <- pub_df %>%
  mutate(
    Country = trimws(Country),
    Year = as.integer(Year),
    Count = as.numeric(Count)
  )

gini_df <- gini_df %>%
  mutate(
    Country = trimws(Country),
    Year = as.integer(Year),
    gini_pub = as.numeric(gini_pub),
    `Income group` = trimws(`Income group`)
  )

#========================
# 3 合并 Gini 和发文量
#========================
df_weight <- gini_df %>%
  inner_join(pub_df, by = c("Country", "Year")) %>%
  filter(
    !is.na(gini_pub),
    !is.na(Count),
    Count > 0
  )

# 可选：先检查一下收入组名字是否真的是 Low income
unique(df_weight$`Income group`)

#========================
# 4 Global 加权 Gini
#========================
g_global_w <- df_weight %>%
  group_by(Year) %>%
  summarise(
    weighted_gini = sum(gini_pub * Count, na.rm = TRUE) / sum(Count, na.rm = TRUE),
    total_pub = sum(Count, na.rm = TRUE),
    n_country = n_distinct(Country),
    .groups = "drop"
  ) %>%
  mutate(Group = "Global")

#========================
# 5 Income group 加权 Gini
#   这里不再去掉 Low income
#========================
g_income_w <- df_weight %>%
  filter(!is.na(`Income group`)) %>%
  group_by(Year, `Income group`) %>%
  summarise(
    weighted_gini = sum(gini_pub * Count, na.rm = TRUE) / sum(Count, na.rm = TRUE),
    total_pub = sum(Count, na.rm = TRUE),
    n_country = n_distinct(Country),
    .groups = "drop"
  ) %>%
  mutate(Group = `Income group`) %>%
  select(-`Income group`)

#========================
# 6 合并
#========================
gini_summary_weighted <- bind_rows(g_global_w, g_income_w) %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "Global",
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      )
    )
  )

#========================
# 7 作图
#========================
peak_year <- 2014

my_cols <- c(
  "Global"              = "grey80",
  "Low income"          = "#F2C14E",
  "Lower middle income" = "#99D096",
  "Upper middle income" = "#53A8E1",
  "High income"         = "#FF8081"
)

p_weighted <- ggplot(
  gini_summary_weighted,
  aes(x = Year, y = weighted_gini, color = Group)
) +
  annotate(
    "rect",
    xmin = peak_year, xmax = 2024,
    ymin = -Inf, ymax = Inf,
    fill = "grey85", alpha = 0.4
  ) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(
    breaks = seq(2000, 2024, by = 5),
    limits = c(2000, 2024),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Year",
    y = "Disciplinary Gini coefficient"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.title = element_text(size = 20, color = "black"),
    axis.text  = element_text(size = 20, color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 18)
  )

print(p_weighted)

ggsave(
  "publication_weighted_gini_with_low_income.png",
  plot = p_weighted,
  width = 7,
  height = 5,
  dpi = 600
)