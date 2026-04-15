################################################################################
# 数据合并脚本
# 功能：合并不同疾病类别的数据，添加疾病来源标识
# 输入：各疾病类别的CSV数据文件
# 输出：合并后的数据文件 original_data.Rdata
################################################################################

# 设置工作路径
setwd('..')  # 设置工作路径为项目根目录

# 加载必要的包
library(dplyr)  # 用于数据处理和合并
library(readr)  # 用于读取CSV文件

# 读取国家排序文件
cat("正在读取国家排序文件...\n")
order <- read_csv("data/processed/order.csv", col_names = FALSE)  # 读取order.csv文件，无列名
names(order)[1] <- "location_name"  # 将第一列重命名为location_name

order_country <- read_csv("data/processed/order_country.csv")  # 读取order_country.csv文件
names(order_country)[1] <- "location_name"  # 将第一列重命名为location_name

order_GBD <- read_csv("data/processed/order_GBD.csv")  # 读取order_GBD.csv文件
names(order_GBD)[1] <- "location_name"  # 将第一列重命名为location_name

# 保存排序数据为Rdata文件
save(order, order_country, order_GBD, file = "data/processed/order.Rdata")
cat("国家排序文件已保存为 data/processed/order.Rdata\n")

################################################################################
# Datacombine函数
# 功能：合并指定类别文件夹内的所有CSV文件
# 参数：
#   - category: 文件夹类别（需在path_mapping中定义）
#   - path_mapping: 命名向量，定义类别与文件夹路径的映射
# 返回值：合并后的数据框
################################################################################
Datacombine <- function(category, path_mapping) {
  # 检查输入有效性
  if (!category %in% names(path_mapping)) {
    stop("无效的类别。有效选项: ", paste(names(path_mapping), collapse = ", "))
  }
  
  # 获取目标文件夹路径
  target_dir <- path_mapping[[category]]
  if (!dir.exists(target_dir)) {
    stop("目录不存在: ", target_dir)
  }
  
  # 获取所有CSV文件路径
  cat(paste("正在查找", target_dir, "中的CSV文件...\n"))
  csv_files <- list.files(
    path = target_dir,
    pattern = "\\.csv$",  # 匹配CSV文件
    full.names = TRUE,  # 返回完整路径
    recursive = FALSE  # 不包含子目录
  )
  
  # 检查是否找到CSV文件
  if (length(csv_files) == 0) {
    warning("在", target_dir, "中未找到CSV文件")
    return(NULL)
  }
  
  cat(paste("找到", length(csv_files), "个CSV文件\n"))
  
  # 批量读取并添加来源标识
  cat("正在读取并处理CSV文件...\n")
  data_list <- lapply(csv_files, function(file) {
    tryCatch({
      # 读取CSV文件，使用UTF-8编码
      df <- read.csv(file, fileEncoding = "UTF-8")
      
      # 添加来源标识
      df %>%
        mutate(
          source_category = category,  # 添加类别标识
          source_file = basename(file)  # 添加文件名标识
        )
    }, error = function(e) {
      # 处理读取错误
      warning("读取失败: ", file, "\n错误: ", e$message)
      return(NULL)
    })
  })
  
  # 移除读取失败的元素（NULL）
  data_list <- Filter(Negate(is.null), data_list)
  cat(paste("成功读取", length(data_list), "个CSV文件\n"))
  
  # 合并数据
  combined_data <- bind_rows(data_list)
  cat(paste("合并后的数据包含", nrow(combined_data), "行\n"))
  
  return(combined_data)
}

# 定义不同类别对应的文件夹路径
path_mapping <- c(
  stomach = "data/raw/胃",      # 胃癌数据路径
  liver = "data/raw/肝",        # 肝癌数据路径
  cavity = "data/raw/口腔",     # 口腔癌数据路径
  colon = "data/raw/结直肠",    # 结直肠癌数据路径
  pancreatic = "data/raw/胰腺"  # 胰腺癌数据路径
)

# 合并数据
cat("开始合并各类疾病数据...\n")
stomach_data <- Datacombine("stomach", path_mapping)  # 合并胃癌数据
liver_data <- Datacombine("liver", path_mapping)      # 合并肝癌数据
cavity_data <- Datacombine("cavity", path_mapping)    # 合并口腔癌数据
colon_data <- Datacombine("colon", path_mapping)      # 合并结直肠癌数据
pancreatic_data <- Datacombine("pancreatic", path_mapping)  # 合并胰腺癌数据

# 保存合并后的数据
cat("正在保存合并后的数据...\n")
save(stomach_data, liver_data, cavity_data, colon_data, pancreatic_data, 
     file = "data/processed/original_data.Rdata")
cat("合并后的数据已保存为 data/processed/original_data.Rdata\n")

# 输出数据统计信息
cat("\n数据合并完成！\n")
cat(paste("胃癌数据: ", nrow(stomach_data), "行\n"))
cat(paste("肝癌数据: ", nrow(liver_data), "行\n"))
cat(paste("口腔癌数据: ", nrow(cavity_data), "行\n"))
cat(paste("结直肠癌数据: ", nrow(colon_data), "行\n"))
cat(paste("胰腺癌数据: ", nrow(pancreatic_data), "行\n"))

