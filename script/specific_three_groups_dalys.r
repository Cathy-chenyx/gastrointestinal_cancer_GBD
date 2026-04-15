################################################################################
# 特定-分三组-jp.r脚本
# 功能：处理不同癌症类型的DALYs数据，对低SDI地区的数据进行分组和聚合
# 输入：各癌症类型的CSV数据文件（measure_2_age_id.csv）
# 输出：按年龄组分组的低SDI地区DALYs数据文件
################################################################################

# 加载必要的包
library(dplyr)  # 用于数据处理
library(readr)  # 用于读取和写入CSV文件

# 指定文件路径（替换为你的文件路径）
file_paths <- c(
  "C:/Users/18511/Desktop/胰腺/measure_2_age_id.csv",   # 胰腺癌DALYs数据
  "C:/Users/18511/Desktop/口腔/measure_2_age_id.csv",   # 口腔癌DALYs数据
  "C:/Users/18511/Desktop/肝/measure_2_age_id.csv",   # 肝癌DALYs数据
  "C:/Users/18511/Desktop/胃/measure_2_age_id.csv", # 胃癌DALYs数据
  "C:/Users/18511/Desktop/结直肠/measure_2_age_id.csv" # 结直肠癌DALYs数据
)

# 对应的癌症名称
cancer_names <- c("Pancreatic cancer", "Lip and oral cavity cancer", "Liver cancer", "Stomach cancer", "Colon and rectum cancer")

# 读取CSV并添加癌症类型（cause_name）
cat("正在读取数据文件...\n")
data_list <- lapply(1:length(file_paths), function(i) {
  df <- read_csv(file_paths[i])  # 读取CSV文件
  df$cause_name <- cancer_names[i]  # 添加癌症名称列
  return(df)
})

# 合并所有数据集
cat("正在合并数据集...\n")
Incidence <- bind_rows(data_list)

# 确保 `val` 是数值，并假设人口数为 100000
Incidence <- Incidence %>%
  mutate(
    val = as.numeric(val),  # 转换为数值类型
    upper = as.numeric(upper),  # 转换为数值类型
    lower = as.numeric(lower),  # 转换为数值类型
    pop = 100000,  # 假设人口数为 10 万（如果数据中没有 pop 列）
    DALYs_cases = val * pop / 100000  # 计算 DALYs 总数（数值）
  ) %>%
  mutate(age_id = case_when(
    age_id %in% c(9, 10, 11) ~ 1,  # 合并 9,10,11 为 1（第一组）
    age_id %in% c(12, 13, 14) ~ 2, # 合并 12,13,14 为 2（第二组）
    TRUE ~ age_id  # 其他 age_id 保持不变
  ))

# 按年龄段、癌症类型、年份、地区进行分组求和
cat("正在计算DALYs数据...\n")
Incidence_agg <- Incidence %>%
  filter(location_name == "Low SDI", metric_name == "Rate") %>%  # 只保留低SDI地区的率数据
  group_by(cause_name, location_name, year, age_id) %>%  # 按癌症类型、地区、年份、年龄组分组
  summarise(
    DALYs_cases = sum(DALYs_cases, na.rm = TRUE),  # 合并不同年龄段的 DALYs 数值
    pop = sum(pop, na.rm = TRUE),  # 计算合并后的总人口
    upper_cases = sum(upper * pop / 100000, na.rm = TRUE),  # 计算上限数值
    lower_cases = sum(lower * pop / 100000, na.rm = TRUE)   # 计算下限数值
  ) %>%
  ungroup() %>%
  mutate(
    val = DALYs_cases / pop * 100000,  # 计算合并后的 DALYs 率
    upper = upper_cases / pop * 100000,  # 计算合并后的上限率
    lower = lower_cases / pop * 100000,  # 计算合并后的下限率
    SE = (upper - lower) / (1.96 * 2)  # 计算标准误差
  )

# 确保输出文件夹存在
output_folder <- "C:/Users/18511/Desktop/特定/DALYs/"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# 遍历 age_id，分别保存不同的 CSV 文件
cat("正在保存数据文件...\n")
for (age in unique(Incidence_agg$age_id)) {
  APC_age <- Incidence_agg %>%
    filter(age_id == age) %>%  # 筛选当前年龄组
    select(cause_name, location_name, year, val, SE)  # 选择需要的列
  
  output_path <- file.path(output_folder, paste0("APC_l_age_id_", age, ".csv"))
  write_csv(APC_age, output_path)  # 写入CSV文件
  cat(paste("已保存:", output_path, "\n"))
}

cat("处理完成，所有 age_id 的数据已保存！\n")
