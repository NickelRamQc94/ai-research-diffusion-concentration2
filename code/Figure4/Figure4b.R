library(tidyverse)
library(readr)
library(ineq)

df_raw <- read_csv("left_file_with_income_group.csv", show_col_types = FALSE)

B <- 1000

# ==============================
# 1️⃣ Global Gini
# ==============================

df_global <- df_raw %>%
  filter(!is.na(Year), !is.na(Total_Paper_Count)) %>%
  group_by(Year) %>%
  summarise(
    Gini = ineq::Gini(Total_Paper_Count, na.rm = TRUE),
    Gini_se = sd(
      replicate(B, ineq::Gini(sample(Total_Paper_Count, replace = TRUE), na.rm = TRUE)),
      na.rm = TRUE
    ),
    Gini_lwr = Gini - Gini_se,
    Gini_upr = Gini + Gini_se,
    Group = "Global",
    .groups = "drop"
  )

# ==============================
# 2️⃣ Income group（去掉 Low income）
# ==============================

df_income <- df_raw %>%
  filter(!is.na(Year), !is.na(`Income group`), !is.na(Total_Paper_Count)) %>%
  mutate(`Income group` = trimws(`Income group`)) %>%
  filter(`Income group` != "Low income") %>%
  group_by(Year, `Income group`) %>%
  summarise(
    Gini = ineq::Gini(Total_Paper_Count, na.rm = TRUE),
    Gini_se = sd(
      replicate(B, ineq::Gini(sample(Total_Paper_Count, replace = TRUE), na.rm = TRUE)),
      na.rm = TRUE
    ),
    Gini_lwr = Gini - Gini_se,
    Gini_upr = Gini + Gini_se,
    .groups = "drop"
  ) %>%
  rename(Group = `Income group`)

# ==============================
# 3️⃣ 合并
# ==============================

df_all <- bind_rows(df_global, df_income) %>%
  mutate(
    Gini_lwr = pmax(Gini_lwr, 0),
    Gini_upr = pmin(Gini_upr, 1),
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

x_breaks <- seq(min(df_all$Year, na.rm = TRUE), max(df_all$Year, na.rm = TRUE), by = 5)
y_breaks <- seq(0.4, 1, by = 0.1)

df_all <- df_all %>%
  mutate(line_w = ifelse(Group == "Global", 3, 2.5))

p <- ggplot(df_all, aes(x = Year, y = Gini, group = Group)) +
  
  geom_line(
    aes(color = Group, linewidth = line_w),
    show.legend = FALSE
  ) +
  
  geom_point(
    aes(color = Group),
    size = 4
  ) +
  
  scale_color_manual(values = c(
    "Global" = "grey80",
    "Lower middle income" = "#99D096",
    "Upper middle income" = "#53A8E1",
    "High income" = "#FF8081"
  )) +
  
  scale_linewidth_identity() +
  
  scale_x_continuous(
    limits = c(2000, max(df_all$Year, na.rm = TRUE)),
    breaks = seq(2000, max(df_all$Year, na.rm = TRUE), by = 5)
  ) +
  scale_y_continuous(breaks = y_breaks) +
  
  coord_cartesian(ylim = c(0.4, 1)) +
  
  labs(x = "Year", y = "Gini", color = NULL) +
  
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    axis.title = element_text(size = 21),
    axis.text = element_text(size = 22),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "right"
  )

print(p)

ggsave("Gini_global_four_income.png", p, width = 10, height = 4.5, dpi = 600)


#======================= 加入low income的 ====================
library(readr)
library(dplyr)
library(ggplot2)
library(ineq)

df_raw <- read_csv("left_file_with_income_group_without_China.csv", show_col_types = FALSE)

B <- 1000

# ==============================
# 1 Global Gini
# ==============================
df_global <- df_raw %>%
  filter(!is.na(Year), !is.na(Total_Paper_Count)) %>%
  group_by(Year) %>%
  summarise(
    Gini = ineq::Gini(Total_Paper_Count, na.rm = TRUE),
    Gini_se = sd(
      replicate(B, ineq::Gini(sample(Total_Paper_Count, replace = TRUE), na.rm = TRUE)),
      na.rm = TRUE
    ),
    Gini_lwr = Gini - Gini_se,
    Gini_upr = Gini + Gini_se,
    Group = "Global",
    .groups = "drop"
  )

# ==============================
# 2 Income group
# ==============================
df_income <- df_raw %>%
  filter(!is.na(Year), !is.na(`Income group`), !is.na(Total_Paper_Count)) %>%
  mutate(`Income group` = trimws(`Income group`)) %>%
  filter(`Income group` %in% c(
    "Low income",
    "Lower middle income",
    "Upper middle income",
    "High income"
  )) %>%
  group_by(Year, `Income group`) %>%
  summarise(
    Gini = ineq::Gini(Total_Paper_Count, na.rm = TRUE),
    Gini_se = sd(
      replicate(B, ineq::Gini(sample(Total_Paper_Count, replace = TRUE), na.rm = TRUE)),
      na.rm = TRUE
    ),
    Gini_lwr = Gini - Gini_se,
    Gini_upr = Gini + Gini_se,
    .groups = "drop"
  ) %>%
  rename(Group = `Income group`)

# ==============================
# 3 合并 + 把 Low income 中等于0的Gini设为NA
# ==============================
df_all <- bind_rows(df_global, df_income) %>%
  mutate(
    # 只针对 Low income，把 0 视为缺失，不画
    Gini = ifelse(Group == "Low income" & Gini <= 1e-8, NA, Gini),
    Gini_se = ifelse(Group == "Low income" & is.na(Gini), NA, Gini_se),
    Gini_lwr = ifelse(Group == "Low income" & is.na(Gini), NA, Gini_lwr),
    Gini_upr = ifelse(Group == "Low income" & is.na(Gini), NA, Gini_upr)
  ) %>%
  mutate(
    Gini_lwr = pmax(Gini_lwr, 0, na.rm = FALSE),
    Gini_upr = pmin(Gini_upr, 1, na.rm = FALSE),
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

x_breaks <- seq(2000,2024, by = 5)
y_breaks <- seq(0.3, 0.9, by = 0.2)

df_all <- df_all %>%
  mutate(line_w = ifelse(Group == "Global", 2, 2))

p <- ggplot(df_all, aes(x = Year, y = Gini, group = Group)) +
  geom_line(
    aes(color = Group, linewidth = line_w),
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  geom_point(
    aes(color = Group),
    size = 3,
    na.rm = TRUE
  ) +
  scale_color_manual(values = c(
    "Global" = "grey80",
    "Low income" = "#F2C14E",
    "Lower middle income" = "#99D096",
    "Upper middle income" = "#53A8E1",
    "High income" = "#FF8081"
  )) +
  scale_linewidth_identity() +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = y_breaks) +
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(x = "Year", y = "Gini without China", color = NULL) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "right"
  )

print(p)

ggsave("Gini_global_four_income_without_China.png", p, width = 10, height = 4.5, dpi = 600)
