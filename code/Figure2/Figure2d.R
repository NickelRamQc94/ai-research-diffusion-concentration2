#=======================提取各个学科的AI开始研究的时间=========================
suppressPackageStartupMessages({
  library(bibliometrix)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
})
in_dir  <- "D:/博后/论文/3/物理科学"  # <- 修改
out_csv <- file.path(in_dir, "first_year_by_country.csv")

files <- list.files(in_dir, pattern = "\\.txt$", full.names = TRUE)
stopifnot(length(files) > 0)

# ==== 2) 读取并转换 ====
# dbsource = "wos", format = "plaintext" 适用于 WOS 导出的 TXT
M <- convert2df(files, dbsource = "wos", format = "plaintext")

# 提取作者国家（bibliometrix 会从 C1/RP 等字段解析）
# 生成一列 AU_CO（Author Countries），多个国家用分号分隔
M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# ==== 3) 拆分国家，规整名称 ====
# 若 AU_CO 为空，先过滤
dat_country <- M2 %>%
  select(UT, PY, AU_CO) %>%
  filter(!is.na(AU_CO), !is.na(PY)) %>%
  mutate(AU_CO = str_split(AU_CO, ";")) %>%
  unnest(AU_CO) %>%
  mutate(country_raw = str_squish(AU_CO))

# 常见国名清洗（可按需扩充）
map_country <- c(
  "Peoples R China" = "China",
  "Hong Kong"       = "China",
  "Taiwan"          = "China",
  "England"         = "United Kingdom",
  "Scotland"        = "United Kingdom",
  "Wales"           = "United Kingdom",
  "Northern Ireland"= "United Kingdom",
  "USA"             = "United States",
  "U.S.A."          = "United States",
  "United States of America" = "United States",
  "Korea"           = "South Korea",
  "ROC"             = "China",
  "Russian Federation" = "Russia"
)

dat_country <- dat_country %>%
  mutate(country = recode(country_raw, !!!map_country, .default = country_raw)) %>%
  # 去掉明显的机构/噪声尾巴，如 "USA."、"China." 等标点
  mutate(country = str_remove_all(country, "[\\.,;]$")) %>%
  mutate(country = str_squish(country))

# ==== 4) 计算各国首次发文年份 ====
first_year_by_country <- dat_country %>%
  group_by(country) %>%
  summarise(first_year = min(as.integer(PY), na.rm = TRUE),
            n_pubs_total = n_distinct(UT), .groups = "drop") %>%
  arrange(first_year, desc(n_pubs_total))

# ==== 5) 导出 & 预览 ====
write_csv(first_year_by_country, out_csv)
print(head(first_year_by_country, 20))
cat("\nSaved to:", out_csv, "\n")

#==================叠加世界地图===============================
suppressPackageStartupMessages({
  library(sf); library(tidyverse); library(stringr); library(scales)
})

# 1) 读入与清洗
first_year <- read_csv("first_year_by_country_clean.csv", show_col_types = FALSE) %>%
  rename(Country = country, First_Year = first_year) %>%
  mutate(
    Country = tools::toTitleCase(str_squish(tolower(Country))),
    Country = recode(Country,
                     "United States Of America"="United States",
                     "Usa"="United States",
                     "Peoples R China"="China","Hong Kong"="China","Taiwan"="China",
                     "England"="United Kingdom","Scotland"="United Kingdom",
                     "Wales"="United Kingdom","Northern Ireland"="United Kingdom",
                     "Republic Of Korea"="South Korea","Korea"="South Korea",
                     .default = Country
    )
  )

# 2) 读取世界地图数据
world_map <- st_read("D:/博后/论文/3/导出文献/世界地图.shp", quiet = TRUE) %>%
  rename(Country = COUNTRY) %>%
  mutate(Country = tools::toTitleCase(str_squish(tolower(Country))))

# 3) 合并数据框，确保有geometry列
world_yr <- first_year %>%
  mutate(
    First_Year = suppressWarnings(as.integer(First_Year)),
    First_Year_Bin = case_when(
      !is.na(First_Year) & First_Year <= 2000 ~ "≤2000",
      !is.na(First_Year) & First_Year >= 2001 & First_Year <= 2005 ~ "2001–2005",
      !is.na(First_Year) & First_Year >= 2006 & First_Year <= 2010 ~ "2006–2010",
      !is.na(First_Year) & First_Year >= 2011 & First_Year <= 2015 ~ "2011–2015",
      !is.na(First_Year) & First_Year >= 2016 & First_Year <= 2020 ~ "2016–2020",
      !is.na(First_Year) & First_Year >= 2021              ~ "≥2021",
      TRUE ~ NA_character_
    ),
    First_Year_Bin = factor(
      First_Year_Bin,
      levels = c("≤2000", "2001–2005", "2006–2010", "2011–2015", "2016–2020", "≥2021")
    )
  )

# Merge the world map with world_yr based on the Country column to include geometry
world_yr_sf <- world_map %>%
  left_join(world_yr, by = "Country")

# 4) 离散配色
bin_colors <- c(
  "≤2000"     = "#4292c6",
  "2001–2005" = "#9ECAE9",
  "2006–2010" = "#7FC97F",
  "2011–2015" = "#C2E699",
  "2016–2020" = "#FDCB82",
  "≥2021"     = "#D96B5F",
  "NA"        = "#D9D9D9"
)

# 5) 绘图（离散填色）
p <- ggplot(world_yr_sf) +
  geom_sf(aes(fill = First_Year_Bin), color = "gray70", linewidth = 0.2) +
  scale_fill_manual(
    values = bin_colors,
    na.value = "gray90",
    name = "First year",
    na.translate = FALSE
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 60)) +
  scale_y_continuous(breaks = seq(-90, 90, 30)) +
  theme(
    legend.position   = "right",
    legend.title      = element_text(size = 20),
    legend.text       = element_text(size = 18),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.6, "cm"),
    legend.key.spacing.y = unit(0.1, "cm"),
    axis.text         = element_text(size = 18, colour = "black"),
    axis.title        = element_text(size = 20),
    axis.ticks        = element_line(colour = "black"),
    axis.line         = element_line(colour = "black"),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid        = element_blank(),
    panel.background  = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# Print the plot
print(p)

ggsave("first_year_world_map.png", p, width = 18, height = 6, dpi = 600)