################################################################################
# 数据预处理脚本
# 功能：解压并处理原始ZIP文件中的CSV数据，筛选特定年龄段和指标的数据
# 输入：ZIP文件（包含CSV数据）
# 输出：按指标类型分类的CSV文件
################################################################################

# 加载必要的包
library(dplyr)  # 用于数据处理和筛选
library(zip)    # 用于处理zip文件
library(tools)  # 用于处理文件路径

# 设置文件夹路径
folder_path <- "../data/raw/肝"  # 存放ZIP文件的文件夹路径（可根据需要修改为其他疾病）
output_folder <- "../data/processed"   # 输出文件夹的路径

# 创建输出文件夹（如果不存在的话）
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)  # recursive = TRUE 确保创建嵌套目录
  cat("创建了输出文件夹：", output_folder, "\n")
}

# 获取所有ZIP文件的文件名
zip_files <- list.files(path = folder_path, pattern = "*.zip", full.names = TRUE)
cat("找到", length(zip_files), "个ZIP文件\n")

# 提取所有ZIP文件中的CSV文件
csv_files <- c()

# 解压所有zip文件中的CSV文件
for (zip_file in zip_files) {
  # 解压ZIP文件到临时目录
  temp_dir <- tempfile()  # 创建临时目录
  dir.create(temp_dir)     # 确保临时目录存在
  unzip(zip_file, exdir = temp_dir)  # 解压文件到临时目录
  
  # 获取解压后所有CSV文件的路径
  temp_csv_files <- list.files(path = temp_dir, pattern = "*.csv", full.names = TRUE)
  
  # 确认解压后的文件路径
  print(paste("从", basename(zip_file), "中解压出", length(temp_csv_files), "个CSV文件"))
  print(temp_csv_files)
  
  # 将CSV文件路径添加到csv_files列表中
  csv_files <- c(csv_files, temp_csv_files)
  
  # 清理临时目录
  unlink(temp_dir, recursive = TRUE)
  cat("已清理临时目录\n")
}

# 创建一个空的数据框来保存筛选后的所有数据
all_filtered_data <- data.frame()

# 循环处理每个CSV文件
for (file in csv_files) {
  # 读取CSV文件
  cat("正在处理文件：", basename(file), "\n")
  data <- read.csv(file)
  
  # 筛选数据: sex_id = 2（女性）, age_name = "15-49 years"（15-49岁年龄段）, 
  # measure_id = 1（死亡率）, 2（DALYs）, 或 6（发病率）
  filtered_data <- data %>%
    filter(sex_id == 2, age_name == "15-49 years", measure_id %in% c(1, 2, 6))
  
  # 将筛选后的数据合并到 all_filtered_data 中
  all_filtered_data <- bind_rows(all_filtered_data, filtered_data)
  cat("已添加", nrow(filtered_data), "行数据\n")
}

# 按 measure_id 筛选并保存不同的CSV文件
for (measure in c(1, 2, 6)) {
  # 筛选出特定 measure_id 的数据
  measure_data <- all_filtered_data %>% filter(measure_id == measure)
  
  # 设置输出路径
  output_csv <- file.path(output_folder, paste0("measure_", measure, "_filtered_data.csv"))
  
  # 将筛选后的数据写入CSV文件
  write.csv(measure_data, output_csv, row.names = FALSE)
  
  # 输出保存信息
  cat(paste("measure_id =", measure, "的数据已保存到", output_csv, "！\n"))
  cat(paste("保存了", nrow(measure_data), "行数据\n"))
}

# 输出处理完成信息
cat("处理完成，所有数据已按 measure_id 分类保存！\n")
cat("总共处理了", nrow(all_filtered_data), "行数据\n")
