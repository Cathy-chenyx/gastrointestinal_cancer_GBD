################################################################################
# 特定 三组 inci jp.r脚本
# 功能：处理不同癌症类型的发病率数据，对高SDI地区的数据进行分组和聚合
# 输入：各癌症类型的CSV数据文件
# 输出：按年龄组分组的高SDI地区发病率数据文件
################################################################################

# 加载必要的包
library(dplyr)  # 用于数据处理
library(readr)  # 用于读取和写入CSV文件

# 指定文件路径（替换为你的文件路径）
file_paths <- c(
  "C:/Users/18511/Desktop/胰腺/measure_6_age_id.csv",   # 胰腺癌数据
  "C:/Users/18511/Desktop/口腔/measure_6_age_id.csv",   # 口腔癌数据
  "C:/Users/18511/Desktop/肝/measure_6_age_id.csv",   # 肝癌数据
  "C:/Users/18511/Desktop/胃/measure_6_age_id.csv", # 胃癌数据
  "C:/Users/18511/Desktop/结直肠/measure_6_age_id.csv" # 结直肠癌数据
)

# 对应的癌症名称
cancer_names <- c("Pancreatic cancer", "Lip and oral cavity cancer", "Liver cancer", "Stomach cancer", "Colon and rectum cancer")

# 读取CSV并添加癌症类型（cause_name）
cat("正在读取数据文件...\n")
data_list <- lapply(1:length(file_paths), function(i) {
  df <- read_csv(file_paths[i], show_col_types = FALSE)  # 读取CSV文件
  df <- df %>% mutate(cause_name = cancer_names[i])  # 添加癌症名称列
  return(df)
})

# 合并所有数据集
cat("正在合并数据集...\n")
Incidence <- bind_rows(data_list)

# 确保 `val` 是数值类型并处理年龄组
Incidence <- Incidence %>%
  mutate(
    val = as.numeric(val),  # 转换为数值类型
    upper = as.numeric(upper),  # 转换为数值类型
    lower = as.numeric(lower)  # 转换为数值类型
  ) %>%
  mutate(
    age_id = case_when(
      age_id %in% c(9, 10, 11) ~ 1,  # 合并 9,10,11 为 1（第一组）
      age_id %in% c(12, 13, 14) ~ 2, # 合并 12,13,14 为 2（第二组）
      TRUE ~ as.numeric(age_id)  # 其他 age_id 保持不变
    )
  )

# 计算 `pop` (人口数)  
cat("正在计算人口数据...\n")
pop_data <- Incidence %>%
  filter(metric_name == "Number") %>%  # 只保留人口数
  select(location_name, year, age_id, cause_name, pop = val)  # 重命名val列为pop

# 合并 `pop` 数据回 `Incidence`
Incidence <- Incidence %>%
  left_join(pop_data, by = c("location_name", "year", "age_id", "cause_name")) 

# 按年龄段、癌症类型、年份、地区进行分组求加权平均
cat("正在计算加权平均发病率...\n")
Incidence_agg <- Incidence %>%
  filter(location_name == "High SDI", metric_name == "Rate") %>%  # 只保留高SDI地区的率数据
  filter(!is.na(pop) & pop > 0) %>%  # 确保 `pop` 非空 & 非零
  group_by(cause_name, location_name, year, age_id) %>%  # 按癌症类型、地区、年份、年龄组分组
  summarise(
    pop = sum(pop, na.rm = TRUE),  # 计算合并后的总人口
    val = sum(val * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE),  # 计算加权平均发病率
    upper = sum(upper * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE),  # 计算上限
    lower = sum(lower * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)   # 计算下限
  ) %>%
  ungroup() %>%
  mutate(
    SE = (upper - lower) / (1.96 * 2)  # 计算标准误差
  )

# 确保输出文件夹存在
output_folder <- "C:/Users/18511/Desktop/特定/Incidence/"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# 遍历 age_id，分别保存不同的 CSV 文件
cat("正在保存数据文件...\n")
for (age in unique(Incidence_agg$age_id)) {
  APC_age <- Incidence_agg %>%
    filter(age_id == age) %>%  # 筛选当前年龄组
    select(cause_name, location_name, year, val, SE)  # 选择需要的列
  
  output_path <- file.path(output_folder, paste0("APC_h_age_id_", age, ".csv"))
  write_csv(APC_age, output_path)  # 写入CSV文件
  cat(paste("已保存:", output_path, "\n"))
}

cat("处理完成，所有 age_id 的数据已保存！\n")
