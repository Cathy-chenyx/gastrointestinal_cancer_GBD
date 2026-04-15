################################################################################
# 年龄组差异分析.R脚本
# 功能：分析不同年龄组的疾病负担差异，生成年龄组分布图和百分比变化图
# 输入：age_summary, all_data, scale_factor
# 输出：年龄组分布图和百分比变化图
################################################################################

# 加载必要的包
library(ggplot2)  # 用于数据可视化
library(dplyr)    # 用于数据处理
library(scales)   # 用于数据格式化

# DALYs年龄组分布图
cat("正在生成DALYs年龄组分布图...\n")
DALYS_age_gruop <- ggplot(age_summary %>% 
         filter(measure_name == "DALYs (Disability-Adjusted Life Years)"), 
       aes(x = age)) +
  # 柱状图（Rate，右轴）
  geom_bar(data = .%>% filter(metric_name == "Rate"),
           stat = "identity", position = "stack",
           aes(y = mean_asr, fill = disease),
           #position = position_dodge(width = 0.7),
           width = 0.6,
           alpha = 0.8
  ) +
  # 折线图（Number，左轴，缩放后）
  geom_line(
    data = . %>% filter(metric_name == "Number"),
    aes(y = mean_asr/scale_factor, group = disease, color = disease), linewidth = 1) +
  # 数据点
  geom_point(aes(y = mean_asr/scale_factor, group = disease, color = disease), size = 2) +
  
  # 双Y轴设置
  scale_y_continuous(
    name = "Number of incident cases in 2021",
    labels = comma,
    sec.axis = sec_axis(
      ~ . * scale_factor,
      name = "Rate of incidence in 2021 ",
      labels = comma
    )
  )+
  # 主题设置
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    #legend.position = "top"
  )+
  
  # 图例排列
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )  

# 发病率年龄组分布图
cat("正在生成发病率年龄组分布图...\n")
Incidence_age_group <- ggplot(age_summary %>% 
         filter(measure_name =="Incidence"), 
       aes(x = age)) +
  # 柱状图（Rate，右轴）
  geom_bar(data = .%>% filter(metric_name == "Rate"),
           stat = "identity", position = "stack",
           aes(y = mean_asr, fill = disease),
           #position = position_dodge(width = 0.7),
           width = 0.6,
           alpha = 0.8
  ) +
  # 折线图（Number，左轴，缩放后）
  geom_line(
    data = . %>% filter(metric_name == "Number"),
    aes(y = mean_asr/scale_factor, group = disease, color = disease), linewidth = 1) +
  # 数据点
  geom_point(aes(y = mean_asr/scale_factor, group = disease, color = disease), size = 2) +
  
  # 双Y轴设置
  scale_y_continuous(
    name = "Number of DALYS in 2021",
    labels = comma,
    sec.axis = sec_axis(
      ~ . * scale_factor,
      name = "Rate of DALYS in 2021",
      labels = comma
    )
  )+
  # 主题设置
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    #legend.position = "top"
  )+
  
  # 图例排列
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )  

# 组合图形
cat("正在组合图形...\n")
combined_plot <- Incidence_age_group + DALYS_age_gruop

# 保存图形
cat("正在保存年龄组分布图...\n")
dir.create("outcome", recursive = TRUE, showWarnings = FALSE)
ggsave("outcome/age_group.png", plot = combined_plot, width = 20, height = 8, dpi = 300)

# 百分比变化图 ------------------------------------------------------------------
cat("正在分析百分比变化...\n")

# 1. 数据预处理 - 计算百分比变化
age_change <- all_data %>%
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
library(patchwork)  # 用于图形组合
combined_plot <- p_incidence_change + p_daly_change +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")  # 设置图例位置

# 5. 保存输出
cat("正在保存百分比变化图...\n")
ggsave("outcome/age_group_percentage_change.png", combined_plot, width = 14, height = 6, dpi = 300)

cat("分析完成！\n")

