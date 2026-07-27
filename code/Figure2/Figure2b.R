library(tidyverse)
library(sf)

# 1️⃣ 读取国家总量数据
pub_total <- read_csv("Country_Total_Publications.csv")

# 2️⃣ 读取世界地图
world <- st_read("D:/博后/论文/3/导出文献/世界地图.shp")

# 3️⃣ 标准化国家名（防止匹配问题）
pub_total <- pub_total %>%
  mutate(
    Country = str_trim(Country),
    Country = str_to_title(Country)
  ) %>%
  rename(COUNTRY = Country)   # 改成和地图字段一致

# ⚠️ 如果你的地图字段不是 COUNTRY，
# 先运行 names(world) 看实际字段名

# 4️⃣ 合并
map_data <- world %>%
  left_join(pub_total, by = "COUNTRY")

# 5️⃣ 画图（推荐 log 变换）
p <- ggplot(map_data) +
  geom_sf(aes(fill = Total_Publications),
          color = "gray80",
          linewidth = 0.2) +
  
  scale_fill_gradientn(
    colours = c("#deebf7", "#9ecae1", "#4292c6", "#08519c"),
    trans = "log10",
    name = "Total publications",
    na.value = "#f0f0f0"
  ) +
  
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 90),
    expand = FALSE
  ) +
  
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60)
  ) +
  
  scale_y_continuous(
    breaks = seq(-60, 90, by = 30)
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title = element_blank()
  )

print(p)

ggsave("global_total_publications.png",
       plot = p,
       width = 14,
       height = 6,
       dpi = 600)
