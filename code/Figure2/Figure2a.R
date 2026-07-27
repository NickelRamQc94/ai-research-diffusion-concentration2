
library(tidyverse)
library(scales)

# 1. 读取数据 (假设文件名为 your_data.csv)
df_raw <- read_csv("All_disciplines_merged.csv")

# 2. 确定 Top 5 国家（按总量排，China 应该是第一）
top_nations_info <- df_raw %>% 
  group_by(Country) %>% 
  summarise(grand_total = sum(Count, na.rm = TRUE)) %>% 
  slice_max(order_by = grand_total, n = 5) %>%
  arrange(desc(grand_total))

top_nations <- top_nations_info %>% pull(Country)

# 3. 处理数据
df_processed <- df_raw %>%
  mutate(Country_Group = ifelse(Country %in% top_nations, Country, "Others")) %>%
  group_by(Year, Country_Group) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")

# 设置因子顺序：China 为第一级
df_processed$Country_Group <- factor(df_processed$Country_Group, 
                                     levels = c(top_nations, "Others"))

# 4. 准备总量折线数据
df_total <- df_processed %>%
  group_by(Year) %>%
  summarise(Global_Total = sum(Total_Count))

# 5. 绘图 (注意 geom_bar 里的 group 参数)
ggplot() +
  geom_bar(data = df_processed, 
           aes(x = Year, y = Total_Count, fill = Country_Group), 
           stat = "identity", 
           position = position_stack(reverse = TRUE), 
           width = 0.8) +
  
  scale_x_continuous(
    breaks = seq(
      floor(min(df_processed$Year, na.rm = TRUE) / 5) * 5 + 5,
      ceiling(max(df_processed$Year, na.rm = TRUE) / 5) * 5 - 5,
      by = 5
    )
  ) +
  
  scale_y_continuous(
    labels = label_number(scale = 1e-3),
    expand = c(0, 0),
    breaks = seq(
      0,
      max(df_total$Global_Total, na.rm = TRUE),
      by = 5000
    )
  ) +
  
  scale_fill_manual(values = c(
    "#FF8081",
    "#53A8E1",
    "#99D096", "#F2C14E", "#bcbddc",
    "#e0e0e0"
  )) +
  
  labs(x = "Year", y = expression("Publication Count (" * 10^3 * ")"), fill = "Country") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 22)
  )

ggsave("pub_country.png",width = 10.5, height = 5.5, dpi = 600)


#============================== 放大1995-2015 
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

df_raw <- read_csv("1996-2015.csv")

# 2. 确定 Top 5 国家
top_nations_info <- df_raw %>% 
  group_by(Country) %>% 
  summarise(grand_total = sum(Count, na.rm = TRUE)) %>% 
  slice_max(order_by = grand_total, n = 5) %>%
  arrange(desc(grand_total))

top_nations <- top_nations_info %>% pull(Country)

# 3. 处理数据
df_processed <- df_raw %>%
  mutate(Country_Group = ifelse(Country %in% top_nations, Country, "Others")) %>%
  group_by(Year, Country_Group) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")

# 设置因子顺序
df_processed$Country_Group <- factor(
  df_processed$Country_Group,
  levels = c(top_nations, "Others")
)

# 4. 绘图
ggplot() +
  geom_bar(
    data = df_processed,
    aes(x = Year, y = Total_Count, fill = Country_Group),
    stat = "identity",
    position = position_stack(reverse = TRUE),
    width = 0.8
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c(
    "#FF8081",
    "#53A8E1",
    "#99D096",
    "#F2C14E",
    "#bcbddc",
    "#e0e0e0"
  )) +
  labs(y = "Publication Count", fill = "Country") +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 28),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),
    legend.position = "none"
  )

ggsave("pub_country_1995-2015.png",width = 10.5, height = 5.5, dpi = 600)
