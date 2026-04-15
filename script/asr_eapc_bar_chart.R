################################################################################
# ASR和EAPC柱状图脚本
# 功能：生成不同疾病的ASR（年龄标准化率）和EAPC（年度百分比变化）的柱状图
# 输入：All_Disease_Results.Rdata数据文件
# 输出：ASR和EAPC的比较柱状图，保存为tiff文件
################################################################################

# 加载必要的包
library(dplyr)      # 用于数据处理
library(tidyr)      # 用于数据整理
library(ggplot2)    # 用于数据可视化
library(tidyverse)  # 用于数据处理
library(patchwork)  # 用于图形组合

# 数据准备 ---------------------------------------------------------

# 加载数据
cat("正在加载数据...\n")
load("data/processed/All_Disease_Results.Rdata")

# 创建数据结构，整理不同疾病的数据
cat("正在整理数据结构...\n")
disease_data <- list(
  "Liver" = list(  # 肝癌
    Deaths = liver[["Death"]],     # 死亡率数据
    DALYs = liver[["DALYS"]],     # DALYs数据
    Incidence = liver[["Incidence"]]  # 发病率数据
  ),
  "Stomach" = list(  # 胃癌
    Deaths = stomach[["Death"]],
    DALYs = stomach[["DALYS"]],
    Incidence = stomach[["Incidence"]]
  ),
  "Colon" = list(  # 结直肠癌
    Deaths = colon[["Death"]],
    DALYs = colon[["DALYS"]],
    Incidence = colon[["Incidence"]]
  ),
  "Pancreatic" = list(  # 胰腺癌
    Deaths = pancreatic[["Death"]],
    DALYs = pancreatic[["DALYS"]],
    Incidence = pancreatic[["Incidence"]]
  ),
  "Cavity" = list(  # 口腔癌
    Deaths = cavity[["Death"]],
    DALYs = cavity[["DALYS"]],
    Incidence = cavity[["Incidence"]]
  )
)

# ASR绘图 -------------------------------------------------------------------

################################################################################
# extract_asr_2021函数
# 功能：提取并整理2021年的ASR数据
# 参数：
#   - disease_list: 包含疾病数据的列表
# 返回值：整理后的ASR数据框
################################################################################
extract_asr_2021 <- function(disease_list) {
  # 创建空数据框存储结果
  asr_df <- data.frame()
  
  # 遍历每个疾病
  for (disease_name in names(disease_list)) {
    # 提取Incidence数据
    incidence <- disease_list[[disease_name]][["Incidence"]][["ASR_2021"]] %>%
      mutate(Disease = disease_name,  # 添加疾病名称
             metric = "Incidence")  # 添加指标类型
    
    # 提取DALYs数据
    dalys <- disease_list[[disease_name]][["DALYs"]][["ASR_2021"]] %>%
      mutate(Disease = disease_name,  # 添加疾病名称
             metric = "DALYs")  # 添加指标类型
    
    # 合并数据
    asr_df <- bind_rows(asr_df, incidence, dalys)
  }
  
  # 按Order.csv排序地区
  asr_df %>%
    mutate(location_name = factor(location_name, 
                                  levels = order$location_name,
                                  ordered = TRUE))
}

# 提取并处理数据
cat("正在提取ASR数据...\n")
plot_data <- extract_asr_2021(disease_data)

# 创建两个子图并组合
cat("正在生成ASR柱状图...\n")

# Incidence子图
p1 <- ggplot(plot_data %>% filter(metric == "Incidence"), 
             aes(x = location_name, y = val, fill = Disease)) +
  geom_col(position = position_dodge(width = 0.8)) +  # 绘制柱状图，使用闪避位置
  labs(
    #title = "Age-standardised incidence rates in 2021",  # 标题（注释掉）
    y = "ASR per 100,000 population",  # y轴标签
    fill = "Disease type"  # 图例标题
  ) +
  scale_fill_brewer(palette = "Set2") +  # 使用Set2调色板
  theme_minimal() +  # 使用简约主题
  theme(
    axis.title.x = element_blank(),  # 不显示x轴标题
    axis.text.x = element_blank(),  # 不显示x轴文本
    plot.margin = margin(b = 2, unit = "mm")  # 设置底部边距
  )

# DALYs子图
p2 <- ggplot(plot_data %>% filter(metric == "DALYs"), 
             aes(x = location_name, y = val, fill = Disease)) +
  geom_col(position = position_dodge(width = 0.8)) +  # 绘制柱状图，使用闪避位置
  labs(
    #title = "Age-standardised DALY rates in 2021",  # 标题（注释掉）
    x = "Countries/region",  # x轴标签
    y = "ASR per 100,000 population"  # y轴标签
  ) +
  scale_fill_brewer(palette = "Set2") +  # 使用Set2调色板
  theme_minimal() +  # 使用简约主题
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # x轴文本旋转45度
    plot.margin = margin(t = 2, unit = "mm")  # 设置顶部边距
  )

# 组合图形
ASR_plot <- p1 / p2 +  # 垂直组合两个图形
  plot_layout(guides = "collect") &  # 收集图例
  theme(legend.position = "bottom")  # 图例放在底部

# 输出图形
#ASR_plot  # 注释掉，不直接显示

# EAPC画图 ------------------------------------------------------------------

################################################################################
# extract_eapc_2021函数
# 功能：提取并整理EAPC数据
# 参数：
#   - disease_list: 包含疾病数据的列表
# 返回值：整理后的EAPC数据框
################################################################################
extract_eapc_2021 <- function(disease_list) {
  # 创建空数据框存储结果
  eapc_df <- data.frame()
  
  # 遍历每个疾病
  for (disease_name in names(disease_list)) {
    # 提取Incidence数据
    incidence <- disease_list[[disease_name]][["Incidence"]][["EAPC"]] %>%
      mutate(Disease = disease_name,  # 添加疾病名称
             metric = "Incidence")  # 添加指标类型
    
    # 提取DALYs数据
    dalys <- disease_list[[disease_name]][["DALYs"]][["EAPC"]] %>%
      mutate(Disease = disease_name,  # 添加疾病名称
             metric = "DALYs")  # 添加指标类型
    
    # 合并数据
    eapc_df <- bind_rows(eapc_df, incidence, dalys)
  }
  
  # 按Order.csv排序地区
  eapc_df %>%
    mutate(location_name = factor(location_name, 
                                  levels = order$location_name,
                                  ordered = TRUE))
}

# 提取并处理数据
cat("正在提取EAPC数据...\n")
plot_data <- extract_eapc_2021(disease_data)

# 创建两个子图并组合
cat("正在生成EAPC柱状图...\n")

# Incidence子图
p1 <- ggplot(plot_data %>% filter(metric == "Incidence"), 
             aes(x = location_name, y = EAPC, fill = Disease)) +
  geom_col(position = position_dodge(width = 0.8)) +  # 绘制柱状图，使用闪避位置
  labs(
    #title = "Age-standardised incidence rates in 2021",  # 标题（注释掉）
    y = "EAPC in ASR of incidence",  # y轴标签
    fill = "Disease type"  # 图例标题
  ) +
  scale_fill_brewer(palette = "Set2") +  # 使用Set2调色板
  theme_minimal() +  # 使用简约主题
  theme(
    axis.title.x = element_blank(),  # 不显示x轴标题
    axis.text.x = element_blank(),  # 不显示x轴文本
    plot.margin = margin(b = 2, unit = "mm")  # 设置底部边距
  )

# DALYs子图
p2 <- ggplot(plot_data %>% filter(metric == "DALYs"), 
             aes(x = location_name, y = EAPC, fill = Disease)) +
  geom_col(position = position_dodge(width = 0.8)) +  # 绘制柱状图，使用闪避位置
  labs(
    #title = "Age-standardised DALY rates in 2021",  # 标题（注释掉）
    x = "Countries/region",  # x轴标签
    y = "EAPC in ASR of DALYS"  # y轴标签
  ) +
  scale_fill_brewer(palette = "Set2") +  # 使用Set2调色板
  theme_minimal() +  # 使用简约主题
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # x轴文本旋转45度
    plot.margin = margin(t = 2, unit = "mm")  # 设置顶部边距
  )

# 组合图形
EAPC_plot <- p1 / p2 +  # 垂直组合两个图形
  plot_layout(guides = "collect") &  # 收集图例
  theme(legend.position = "bottom")  # 图例放在底部

# 输出图形
#EAPC_plot  # 注释掉，不直接显示

# 保存图形
cat("正在保存图形...\n")
dir.create("outcome", recursive = TRUE, showWarnings = FALSE)
ggsave("outcome/asr_comparison.tiff", ASR_plot, 
       width = 12, height = 8, dpi = 300, compression = "lzw")
ggsave("outcome/EAPC_comparison.tiff", EAPC_plot, 
       width = 12, height = 8, dpi = 300, compression = "lzw")

cat("图形已保存为 outcome/asr_comparison.tiff 和 outcome/EAPC_comparison.tiff\n")
