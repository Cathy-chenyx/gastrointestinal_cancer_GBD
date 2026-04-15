################################################################################
# age_group_disparity_last.R脚本
# 功能：分析不同年龄组的疾病负担差异，生成年龄组分布和变化趋势图
# 输入：original_data.Rdata
# 输出：年龄组分布图和百分比变化图
################################################################################

# 安装必要的包（如果未安装）
# install.packages("openxlsx")

# 设置工作路径
setwd('..')  # 设置工作路径为项目根目录

# 加载必要的包
library(dplyr)                           # 用于数据处理
library(ggplot2)                         # 用于数据可视化
library(openxlsx)                        # 用于Excel文件处理
library(tidyverse)                       # 用于数据处理
library(scales)                          # 用于数据格式化

# 加载数据
cat("正在加载数据...\n")
load("data/processed/original_data.Rdata")

# 合并所有疾病数据
cat("正在合并疾病数据...\n")
all_data <- bind_rows(
  stomach_data, liver_data, colon_data, pancreatic_data, cavity_data,
  .id = "disease"  # 自动添加疾病来源标识
) %>%
  mutate(
    # 重命名疾病标识
    disease = case_when(
      disease == "1" ~ "Stomach Cancer",
      disease == "2" ~ "Liver Cancer",
      disease == "3" ~ "Colon Cancer",
      disease == "4" ~ "Pancreatic Cancer",
      disease == "5" ~ "Cavity Cancer"
    ),
    # 转换年龄组为有序因子
    age = factor(age_name, 
                 # levels = c("15-19years", "20-24years", "25-29years",
                 #    "30-34years", "35-39years","40-45years","Age-standardized"),
                 ordered = TRUE)
  ) %>%
  filter(
    sex_name == "Female",      # 筛选女性数据
    # year == 2021,         # 选择目标年份
    # metric_name == "Rate" ,     # 使用标准化率
    age != "Age-standardized"  # 排除年龄标准化数据
  )

# 检查数据完整性
cat("正在检查数据完整性...\n")
# 检查年龄组完整性
print("年龄组：")
print(unique(all_data$age))

# 验证疾病分类
print("疾病分类：")
print(unique(all_data$disease))

# 检查缺失值
print("缺失值统计：")
summary(all_data$val)

# 年龄组差异分析 -----------------------------------------------------------------
cat("正在分析年龄组差异...\n")
age_summary <- all_data %>%
  filter(year == 2021, age != "15-49 years") %>%  # 筛选2021年数据，排除15-49岁年龄组
  group_by(disease, measure_name, age, metric_name) %>%  # 分组
  summarise(
    mean_asr = mean(val, na.rm = TRUE),  # 计算平均值
    sd_asr = sd(val, na.rm = TRUE),      # 计算标准差
    .groups = "drop"  # 取消分组
  )

# 设置缩放因子，用于双轴图表
scale_factor <- 20

# 1. 定义统一配色方案
disease_colors <- c(
  "Stomach Cancer" = "#E41A1C",  # 红色
  "Liver Cancer" = "#377EB8",   # 蓝色
  "Colon Cancer" = "#4DAF4A",   # 绿色
  "Pancreatic Cancer" = "#984EA3",  # 紫色
  "Cavity Cancer" = "#FF7F00"    # 橙色
)

################################################################################
# create_age_plot函数
# 功能：创建年龄组分布图
# 参数：
#   - data: 输入数据
#   - measure_type: 测量类型（如"Incidence"）
#   - y1_label: 主Y轴标签
#   - y2_label: 次Y轴标签
# 返回值：ggplot对象
################################################################################
create_age_plot <- function(data, measure_type, y1_label, y2_label) {
  ggplot(data %>% filter(measure_name == measure_type), 
         aes(x = age)) +
    # 绘制柱状图（Rate数据）
    geom_bar(
      data = . %>% filter(metric_name == "Rate"),
      aes(y = mean_asr, fill = disease),
      stat = "identity", position = "stack",
      width = 0.6, alpha = 0.8
    ) +
    # 绘制折线图（Number数据，缩放后）
    geom_line(
      data = . %>% filter(metric_name == "Number"),
      aes(y = mean_asr/scale_factor, group = disease, color = disease),
      linewidth = 1
    ) +
    # 绘制数据点
    geom_point(
      data = . %>% filter(metric_name == "Number"),
      aes(y = mean_asr/scale_factor, color = disease),
      size = 2
    ) +
    # 设置双Y轴
    scale_y_continuous(
      name = y1_label,
      labels = comma,
      sec.axis = sec_axis(~ . * scale_factor, name = y2_label)
    ) +
    # 设置颜色
    scale_fill_manual(values = disease_colors) +
    scale_color_manual(values = disease_colors) +
    # 设置主题
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(x = "Age Group")
}

# 3. 生成子图
cat("正在生成子图...\n")
p_incidence <- create_age_plot(age_summary, "Incidence", "Number of incident cases in 2021", "Rate of incidence in 2021") +
  ggtitle("A) Incidence")

p_dalys <- create_age_plot(age_summary, "DALYs (Disability-Adjusted Life Years)", "Number of DALYS in 2021", "Rate of DALYS in 2021") +
  ggtitle("B) DALYs")

# 4. 组合图形并添加图例
cat("正在组合图形...\n")
library(patchwork)  # 用于图形组合
final_plot <- wrap_plots(p_incidence, p_dalys, nrow = 1) +
  plot_layout(guides = "collect") &  # 收集图例
  scale_fill_manual(values = disease_colors) &
  scale_color_manual(values = disease_colors) &
  theme(legend.position = "bottom")  # 设置图例位置

# 6. 保存图形
cat("正在保存年龄组分布图...\n")
dir.create("outcome", recursive = TRUE, showWarnings = FALSE)
ggsave("outcome/age_group_plot.png", final_plot, width = 16, height = 8, dpi = 300)

# 百分比变化图 ------------------------------------------------------------------
cat("正在分析百分比变化...\n")

# 1. 数据预处理 - 计算百分比变化
age_change <- all_data %>%
  filter(age != "15-49 years") %>%  # 排除15-49岁年龄组
  group_by(year, disease, measure_name, age, metric_name) %>%  # 分组
  summarise(
    mean_asr = mean(val, na.rm = TRUE),  # 计算平均值
    sd_asr = sd(val, na.rm = TRUE),      # 计算标准差
    .groups = "drop"  # 取消分组
  )

# 计算1990-2021年的百分比变化
change_data <- age_change %>%
  filter(measure_name %in% c("Incidence", "DALYs (Disability-Adjusted Life Years)"),
         metric_name == "Rate") %>%  # 筛选Rate数据
  select(disease, age, year, measure_name, mean_asr) %>%  # 选择需要的列
  pivot_wider(names_from = year, values_from = mean_asr) %>%  # 宽格式转换
  mutate(
    pct_change = (`2021` / `1990` - 1) * 100,  # 计算百分比变化
    # 重新排序年龄组
    age = factor(age, levels = c("15-19 years", "20-24 years", "25-29 years",
                                 "30-34 years", "35-39 years", "40-44 years", "45-49 years", "15-49 years"))
  )

# 2. 发病率变化图 (C)
cat("正在生成发病率变化图...\n")
p_incidence_change <- ggplot(
  change_data %>% filter(measure_name == "Incidence"),
  aes(x = age, y = pct_change, group = disease, color = disease)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +  # 添加零线
  geom_line(linewidth = 1.2) +  # 绘制折线
  geom_point(size = 3) +  # 绘制数据点
  labs(
    title = "C) Percentage change in incidence rate (1990-2021)",
    y = "Percentage change (%)",
    x = "Age group",
    color = "Cancer type"
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +  # 百分比标签
  scale_color_brewer(palette = "Set1") +  # 颜色方案
  theme_minimal() +  # 主题
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill = alpha("white", 0.8))
  )

# 3. DALY率变化图 (D)
cat("正在生成DALY率变化图...\n")
p_daly_change <- ggplot(
  change_data %>% filter(measure_name == "DALYs (Disability-Adjusted Life Years)"),
  aes(x = age, y = pct_change, group = disease, color = disease)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +  # 添加零线
  geom_line(linewidth = 1.2) +  # 绘制折线
  geom_point(size = 3) +  # 绘制数据点
  labs(
    title = "D) Percentage change in DALY rate (1990-2021)",
    y = "Percentage change (%)",
    x = "Age group",
    color = "Cancer type"
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +  # 百分比标签
  scale_color_brewer(palette = "Set1") +  # 颜色方案
  theme_minimal() +  # 主题
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # 共用图例
  )

# 4. 组合图形
cat("正在组合百分比变化图...\n")
combined_plot <- p_incidence_change + p_daly_change +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")  # 设置图例位置

# 5. 保存输出
cat("正在保存百分比变化图...\n")
ggsave("outcome/age_group_percentage_changes.png", combined_plot, width = 14, height = 6, dpi = 300)

cat("分析完成！\n")


