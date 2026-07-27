library(tidyverse)
library(stringr)

# 设置路径
wos_dir <- "D:/博后/论文/3/物理科学"  # 替换为你的文件夹路径
output_dir <- "D:/博后/论文/3/物理科学/output_csvs"  # 替换为输出路径
dir.create(output_dir, showWarnings = FALSE)

# 获取所有txt文件
files <- list.files(wos_dir, pattern = "\\.txt$", full.names = TRUE)

# 初始化总表
all_data <- tibble()

# 提取每个文件内容
for (file in files) {
  lines <- readLines(file, encoding = "UTF-8")
  
  # 按记录块分组（每条记录以 "PT " 开头）
  record_indices <- grep("^PT ", lines)
  record_indices <- c(record_indices, length(lines) + 1)
  
  for (i in seq_along(record_indices[-length(record_indices)])) {
    rec <- lines[record_indices[i]:(record_indices[i + 1] - 1)]
    
    # 提取出版年份（PY）和通讯作者地址（RP）
    py_line <- rec[str_detect(rec, "^PY ")]
    rp_lines <- rec[str_detect(rec, "^RP ")]
    
    if (length(py_line) > 0 && length(rp_lines) > 0) {
      py <- str_trim(str_remove(py_line, "^PY\\s+"))
      rp_text <- str_remove(rp_lines, "^RP\\s+")
      
      # 可能存在多个地址（以 ; 分隔）
      addresses <- unlist(str_split(rp_text, ";"))
      countries <- str_extract(addresses, "[A-Za-z ]+\\.$") %>%
        str_remove("\\.") %>%
        str_trim()
      
      countries <- countries[!is.na(countries)]
      
      if (length(countries) > 0) {
        first_country <- countries[1]
        all_data <- bind_rows(all_data, tibble(Year = py, Country = first_country))
      }
    }
  }
}

# 清洗年份和国家名
all_data <- all_data %>%
  filter(!is.na(Year), !is.na(Country)) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Year >= 1900 & Year <= 2025)  # 可根据需要修改年份范围

# 分组统计
pub_stats <- all_data %>%
  group_by(Country, Year) %>%
  summarise(Count = n(), .groups = "drop")

# 保存为每个国家一个csv
countries <- unique(pub_stats$Country)

for (c in countries) {
  df <- pub_stats %>% filter(Country == c)
  write_csv(df, file.path(output_dir, paste0(c, "_publications.csv")))
}
