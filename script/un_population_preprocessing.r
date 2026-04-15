################################################################################
# 联合国人口数据预处理.r脚本
# 功能：处理联合国人口数据，将宽格式数据转换为长格式，并添加年龄组名称和ID
# 输入：data2.csv（联合国人口数据，宽格式）
# 输出：转换后的长格式数据，可保存为CSV文件
################################################################################

# 加载必要的库
library(dplyr)  # 用于数据处理
library(tidyr)  # 用于数据重塑（如pivot_longer）

# 读取数据
cat("正在读取人口数据...\n")
data <- read.csv("C:/Users/18511/Desktop/data2.csv")  # 替换为实际的文件路径

# 使用pivot_longer转置数据，将宽格式转换为长格式
cat("正在转换数据格式...\n")
transposed_data <- data %>%
  pivot_longer(cols = starts_with("Age"),  # 转换所有以Age开头的列
               names_to = "age_id",  # 列名转换为age_id列
               values_to = "age_value") %>%  # 列值转换为age_value列
  mutate(
    # 添加年龄组名称
    age_name = case_when(
      age_id == "Age_15_19" ~ "15-19 years",
      age_id == "Age_20_24" ~ "20-24 years",
      age_id == "Age_25_29" ~ "25-29 years",
      age_id == "Age_30_34" ~ "30-34 years",
      age_id == "Age_35_39" ~ "35-39 years",
      age_id == "Age_40_44" ~ "40-44 years",
      age_id == "Age_45_49" ~ "45-49 years",
      TRUE ~ age_id  # 如果有其他不在列表中的age_id，保留原值
    ),
    # 转换age_id为数字格式
    age_id = recode(age_id,
                    Age_15_19 = 8,  # 15-19岁对应age_id 8
                    Age_20_24 = 9,  # 20-24岁对应age_id 9
                    Age_25_29 = 10,  # 25-29岁对应age_id 10
                    Age_30_34 = 11,  # 30-34岁对应age_id 11
                    Age_35_39 = 12,  # 35-39岁对应age_id 12
                    Age_40_44 = 13,  # 40-44岁对应age_id 13
                    Age_45_49 = 14)  # 45-49岁对应age_id 14
  )

# 查看转换后的数据
cat("转换后的数据预览：\n")
print(transposed_data)

# 保存为新的CSV文件
output_path <- "C:/Users/18511/Desktop/path_to_save_file.csv"
write.csv(transposed_data, output_path, row.names = FALSE)
cat(paste("数据已保存到:", output_path, "\n"))

cat("人口数据预处理完成！\n")
