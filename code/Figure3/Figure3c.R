library(tidyverse)
library(mblm)   # ✅ 替代 sens.slope() 的更稳健版本

# === 1️⃣ 读取数据 ===
gini_data <- read_csv("country_year_gini.csv", show_col_types = FALSE)

gini_data <- gini_data %>%
  rename(
    Country = Country,
    Year = Year,
    Gini = gini_pub
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(Gini)

# === 2️⃣ 定义计算函数 ===
calc_mblm <- function(df) {
  if (nrow(df) < 3) return(NA)
  tryCatch({
    model <- mblm(Gini ~ Year, data = df, repeated = FALSE)
    slope <- coef(model)[2]  # 提取斜率
    return(slope)
  }, error = function(e) NA)
}

# === 3️⃣ 按国家计算 ===
slope_results <- gini_data %>%
  group_by(Country) %>%
  summarise(
    sen_slope = calc_mblm(cur_data()),
    mean_gini = mean(Gini, na.rm = TRUE),
    sd_gini   = sd(Gini, na.rm = TRUE),
    n_years   = n_distinct(Year),
    .groups = "drop"
  )

# === 4️⃣ 导出结果 ===
write_csv(slope_results, "gini_slope_mblm.csv")
cat("✅ 已计算完成：gini_slope_mblm.csv\n")

# === 5️⃣ 查看前几行结果 ===
print(head(slope_results, 10))

#===================然后叠加世界地图================

slope_data <- read_csv("gini_slope_mblm.csv")  # 含 sen_slope_% 的文件
world <- st_read("D:/博后/论文/3/导出文献/世界地图.shp")  # 世界地图 Shapefile

# === 2️⃣ 数据清洗 ===
slope_data <- slope_data %>%
  mutate(
    Country = str_trim(Country),
    Country = str_to_title(Country)
  ) %>%
  rename(COUNTRY = Country)

# === 3️⃣ 合并地图和数据 ===
map_data <- world %>%
  left_join(slope_data, by = "COUNTRY")

# === 4️⃣ 绘制地图 ===
p <- ggplot(map_data) +
  geom_sf(aes(fill = sen_slope_pct), color = "gray60", linewidth = 0.3) +
  scale_fill_gradient2(
    name = "Sen’s slope (% / year)",
    low = "#1f78b4",   # 蓝色：不平衡下降
    mid = "white",     
    high = "#FF8081",  # 红色：不平衡上升
    midpoint = 0,      # 0为中点
    limits = c(-2, 2), # 你可以调整范围，比如 ±2%
    oob = scales::squish,
    na.value = "white"
  ) +
  labs(
    title = "Global Trends in Disciplinary Inequality (Sen’s slope % per year)",
    subtitle = "Positive: inequality increasing | Negative: inequality decreasing"
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p)

# === 5️⃣ 保存图像 ===
ggsave("global_gini_trend_map.png",
       plot = p, width = 12, height = 6, dpi = 600)