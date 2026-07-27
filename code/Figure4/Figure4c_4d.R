#================ 计算不同年份的share ==============================
library(dplyr)
library(readr)

# 读入数据
df_total <- read_csv("National_Annual_All_Disciplines_Summary.csv",
                     show_col_types = FALSE)

# 计算百分比 share
share_df <- df_total %>%
  filter(Year %in% c(2014, 2023)) %>%
  group_by(Year) %>%
  mutate(
    total_year = sum(Total_Paper_Count, na.rm = TRUE),
    share_percent = round(Total_Paper_Count / total_year * 100, 2)
  ) %>%
  ungroup() %>%
  select(Country_std, Year, Total_Paper_Count, share_percent) %>%
  arrange(Year, desc(share_percent))

# 输出
write_csv(share_df, "country_share_percent_2014_2023.csv")

cat("Saved: country_share_percent_2014_2023.csv\n")

#======================== 叠加世界地图=============================
#======================== 叠加世界地图=============================
#======================== 叠加世界地图=============================
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# === 1️⃣ 读数据 ===
share_data <- read_csv("country_share_percent_2014_2023.csv",
                       show_col_types = FALSE)

world <- st_read("D:/博后/论文/3/导出文献/世界地图.shp")

# === 2️⃣ 统一国家名称格式 ===
share_data <- share_data %>%
  mutate(
    Country_std = str_trim(Country_std),
    Country_std = str_to_title(Country_std)
  ) %>%
  rename(COUNTRY = Country_std)

world <- world %>%
  mutate(
    COUNTRY = str_trim(COUNTRY),
    COUNTRY = str_to_title(COUNTRY)
  )

# === 3️⃣ 筛选 2020 年 ===
share_2023 <- share_data %>%
  filter(Year == 2014)

# === 4️⃣ 合并 ===
map_data <- world %>%
  left_join(share_2023, by = "COUNTRY")

# === 5️⃣ 画图 ===
library(ggplot2)
library(sf)

# 自定义渐变色
my_cols <- c("#eff3ff",
             "#bdd7e7",
             "#6baed6",
             "#3182bd",
             "#08519c")

p <- ggplot(map_data) +
  
  geom_sf(aes(fill = share_percent),
          color = "grey40",
          size = 0.1) +
  
  # 自定义渐变
  scale_fill_gradientn(
    colours = my_cols,
    na.value = "grey90",
    name = "Share (%)"
  ) +
  
  # 经纬度刻度
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60)
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30)
  ) +
  
  theme_minimal() +
  theme(
    # 去掉所有背景网格线
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 四周黑框
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    ),
    
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 21),
    
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.position = "bottom"
  )+
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-75, 95),
    expand = FALSE
  )

print(p)

ggsave("world_share_2014.png", p, width = 10, height = 8, dpi = 600)
