################################################################################
# 年龄组差异分析脚本
# 功能：分析不同年龄组的疾病负担差异，特别是针对女性2021年的数据
# 输入：original_data.Rdata数据文件
# 输出：年龄组差异分析结果和相关图形
################################################################################

# 相关包准备
#install.packages("openxlsx")  # 安装openxlsx包（如果未安装）

# 设置工作路径
setwd('~/Documents/学习相关/统计建模/统计建模大赛') 

# 加载必要的包
library(dplyr)                  # 用于数据处理
library(ggplot2)                # 用于数据可视化
library(openxlsx)               # 用于Excel文件操作
library(tidyverse)              # 用于数据处理

# 加载数据
cat("正在加载数据...\n")
load("data/processed/original_data.Rdata")

# 数据预处理 ---------------------------------------------------------

# 合并不同疾病的数据
cat("正在合并数据...\n")
all_data <- bind_rows(
  stomach_data, liver_data, colon_data, pancreatic_data, cavity_data,
  .id = "disease"  # 自动添加疾病来源标识
) %>%
  mutate(
    disease = case_when(  # 重命名疾病标识
      disease == "1" ~ "Stomach Cancer",  # 胃癌
      disease == "2" ~ "Liver Cancer",    # 肝癌
      disease == "3" ~ "Colon Cancer",    # 结直肠癌
      disease == "4" ~ "Pancreatic Cancer",  # 胰腺癌
      disease == "5" ~ "Cavity Cancer"     # 口腔癌
    ),
    # 转换年龄组为有序因子
    age = factor(age_name, 
                 #levels = c("15-19years", "20-24years", "25-29years",
                        #    "30-34years", "35-39years","40-45years","Age-standardized"),
                 ordered = TRUE)
  ) %>%
  filter(
    sex_name == "Female",      # 筛选女性数据
    year == 2021,         # 选择目标年份
    #metric_name == "Rate" ,     # 使用标准化率
    age !="Age-standardized"  # 排除年龄标准化数据
  )

# 数据质量检查 ---------------------------------------------------------

# 检查年龄组完整性
cat("检查年龄组完整性...\n")
print(unique(all_data$age))

# 验证疾病分类
cat("验证疾病分类...\n")
print(unique(all_data$disease))

# 检查缺失值
cat("检查缺失值...\n")
summary(all_data$val)

# 年龄组差异分析 -----------------------------------------------------------------

cat("进行年龄组差异分析...\n")
age_summary <- all_data %>%
  group_by(disease, measure_name, age, metric_name) %>%
  summarise(
    mean_asr = mean(val, na.rm = TRUE),  # 计算平均ASR
    sd_asr = sd(val, na.rm = TRUE),      # 计算标准差
    .groups = "drop"  # 取消分组
  )

# 输出示例
cat("输出示例数据...\n")
age_summary %>%
  filter(disease == "Liver Cancer", measure_name == "Incidence")

# 分疾病-指标的年龄趋势图
cat("生成年龄趋势图...\n")
ggplot(age_summary, aes(x = age, y = mean_asr, group = disease)) +
  geom_line(aes(color = disease), linewidth = 1) +  # 绘制线条
  geom_point(aes(color = disease), size = 2) +      # 绘制点
  geom_ribbon(aes(ymin = mean_asr - sd_asr, 
                  ymax = mean_asr + sd_asr, 
                  fill = disease),
              alpha = 0.2) +  # 绘制误差带
  facet_grid(measure_name ~ ., scales = "free_y") +  # 按指标分面
  labs(title = "Age-specific Disease Burden Patterns (Female, 2021)",
       x = "Age Group", 
       y = "Age-Standardized Rate",
       color = "Cancer Type",
       fill = "Cancer Type") +
  theme_bw() +  # 使用白色背景主题
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # x轴文本旋转45度
    legend.position = "bottom"  # 图例放在底部
  )

# 年龄组排序热图 -----------------------------------------------------------------

cat("生成年龄组排序热图...\n")
ggplot(age_summary, aes(x = age, y = disease, fill = mean_asr)) +
  geom_tile(color = "white") +  # 绘制热图
  scale_fill_viridis_c(option = "magma", direction = -1) +  # 使用viridis颜色刻度
  facet_grid(measure_name ~ ., scales = "free") +  # 按指标分面
  labs(title = "Age Group Burden Ranking",
       x = "Age Group",
       y = "Cancer Type",
       fill = "ASR") +
  theme_minimal()  # 使用简约主题

# 统计检验 --------------------------------------------------------------------

# Kruskal-Wallis检验（非参数方法）
cat("进行Kruskal-Wallis检验...\n")
library(rstatix)  # 加载统计检验包

all_data %>%
  group_by(disease, measure_name) %>%
  kruskal_test(val ~ age) %>%  # 进行Kruskal-Wallis检验
  adjust_pvalue(method = "BH") %>%  # 使用Benjamini-Hochberg方法调整p值
  add_significance("p.adj")  # 添加显著性标记

# 关键年龄识别
cat("识别关键年龄...\n")
peak_ages <- age_summary %>%
  group_by(disease, measure_name) %>%
  slice_max(mean_asr, n = 1) %>%  # 选择ASR最高的年龄组
  select(disease, measure_name, peak_age = age)  # 重命名列

# 输出峰值年龄
cat("输出峰值年龄...\n")
print(peak_ages, n = Inf)

# 年轻女士分析 --------------------------------------------------------------------

cat("分析年轻女士数据...\n")
young_data <- all_data %>%
  filter(age %in% c("15-19", "20-24", "25-29"))  # 筛选年轻年龄组

# 计算AAPC
cat("计算AAPC...\n")
library(joinpointR)  # 加载joinpointR包

# 示例：肝癌发病率年轻组趋势
liver_young <- young_data %>%
  filter(disease == "Liver Cancer", measure == "Incidence")

# 拟合joinpoint模型
jp_model <- joinpoint(liver_young, 
                      year = "year", 
                      obs = "val", 
                      groups = "age")

# 输出模型摘要
summary(jp_model)

# 绘制模型结果
plot(jp_model) + 
  labs(title = "Liver Cancer Incidence Trend in Young Females")