################################################################################
# 疾病成分比.R脚本
# 功能：计算并可视化不同地区和年份的癌症发病率成分比
# 输入：original_data.Rdata（包含各癌症类型的数据）
# 输出：percent.tiff（癌症成分比的堆叠柱状图）
################################################################################

# 设置工作路径
setwd('..')  # 设置工作路径为项目根目录

# 加载必要的包
library(ggplot2)  # 用于数据可视化
library(ggsci)    # 用于科学期刊风格的配色

# 加载数据
load("data/processed/original_data.Rdata")

# 合并所有癌症类型的数据
cat("正在合并癌症数据...\n")
LC <- bind_rows(
  stomach_data, liver_data, colon_data, pancreatic_data, cavity_data,  # 合并胃癌、肝癌、结直肠癌、胰腺癌、口腔癌数据
) %>%
  filter(
    measure_name == 'Incidence'  # 只保留发病率数据
  )

# 与地区顺序数据合并
LC <- LC %>%
  inner_join(order, by = "location_name")

# 2021年癌症成分比计算 --------------------------------------------------------
cat("正在计算2021年癌症成分比...\n")

# 提取2021年全年龄段的绝对发病数
case <- subset(LC, LC$year == 2021 & 
                 LC$age_id == 24 &  # 24表示全年龄段
                 LC$metric_name == 'Number' &  # Number表示绝对数
                 LC$measure_name == 'Incidence')  # Incidence表示发病率

# 构建空数据集，用于存储各地区各癌症类型的成分比
LC_percent <- data.frame(
  location_name = rep(unique(LC$location_name), each = length(unique(LC$cause_name))),
  cause_name = rep(unique(LC$cause_name), length(unique(LC$location_name))),
  percent = rep(NA, times = length(unique(LC$cause_name)) * length(unique(LC$location_name)))
)

a <- unique(LC$location_name)  # 地区列表
b <- unique(LC$cause_name)  # 癌症类型列表

# 循环计算每个地区各癌症类型的成分比
for (i in 1:length(unique(LC$location_name))) {  
  location_name_i <- a[i]  # 当前地区名称
  data <- subset(case, case$location_name == location_name_i)[, c(4, 10, 14)]  # 选择地区名称、癌症类型、数值列
  sum <- sum(data$val)  # 计算该地区总发病数
  data$percent <- data$val / sum  # 计算各癌症类型的成分比
  data <- unique(data[, -3])  # 去重，保留地区名称、癌症类型和成分比
  
  # 填充成分比数据到LC_percent
  for (j in 1:length(unique(LC$cause_name))) { 
    cause_name_j <- b[j]  # 当前癌症类型
    LC_percent[which(LC_percent$location_name == location_name_i & LC_percent$cause_name == cause_name_j), 3] <- data$percent[which(data$cause_name == cause_name_j)]
  }
}

# 保存2021年成分比数据
LC_2021 <- LC_percent
LC_2021$year <- 2021  # 添加年份列

# 1990年癌症成分比计算 --------------------------------------------------------
cat("正在计算1990年癌症成分比...\n")

# 提取1990年全年龄段的绝对发病数
case <- subset(LC, LC$year == 1990 & 
                 LC$age_id == 24 &  # 24表示全年龄段
                 LC$metric_name == 'Number' &  # Number表示绝对数
                 LC$measure_name == 'Incidence')  # Incidence表示发病率

# 构建空数据集，用于存储各地区各癌症类型的成分比
LC_percent <- data.frame(
  location_name = rep(unique(LC$location_name), each = length(unique(LC$cause_name))),
  cause_name = rep(unique(LC$cause_name), length(unique(LC$location_name))),
  percent = rep(NA, times = length(unique(LC$cause_name)) * length(unique(LC$location_name)))
)

a <- unique(LC$location_name)  # 地区列表
b <- unique(LC$cause_name)  # 癌症类型列表

# 循环计算每个地区各癌症类型的成分比
for (i in 1:length(unique(LC$location_name))) {  
  location_name_i <- a[i]  # 当前地区名称
  data <- subset(case, case$location_name == location_name_i)[, c(4, 10, 14)]  # 选择地区名称、癌症类型、数值列
  sum <- sum(data$val)  # 计算该地区总发病数
  data$percent <- data$val / sum  # 计算各癌症类型的成分比
  data <- unique(data[, -3])  # 去重，保留地区名称、癌症类型和成分比
  
  # 填充成分比数据到LC_percent
  for (j in 1:length(unique(LC$cause_name))) { 
    cause_name_j <- b[j]  # 当前癌症类型
    LC_percent[which(LC_percent$location_name == location_name_i & LC_percent$cause_name == cause_name_j), 3] <- data$percent[which(data$cause_name == cause_name_j)]
  }
}

# 保存1990年成分比数据
LC_1990 <- LC_percent
LC_1990$year <- 1990  # 添加年份列

# 合并1990年和2021年数据
LC_1990_2021 <- rbind(LC_1990, LC_2021)
LC_1990_2021$text <- as.character(round(LC_1990_2021$percent * 100, 1))  # 计算百分比文本

# 设置因子水平，控制绘图顺序
LC_1990_2021$location_name <- factor(LC_1990_2021$location_name, 
                                levels = order$location_name, 
                                ordered = TRUE)
LC_1990_2021$cause_name <- factor(LC_1990_2021$cause_name, 
                             levels = c("Stomach cancer",
                                      "Liver cancer",
                                      "Colon and rectum cancer",
                                      "Pancreatic cancer",
                                      "Lip and oral cavity cancer"), 
                             ordered = TRUE)

# 绘制成分比堆叠柱状图
cat("正在绘制成分比图...\n")
percent <- ggplot(LC_1990_2021, aes(x = reorder(location_name, percent), y = percent, fill = cause_name)) +
  geom_col(position = "fill") +  # 堆叠柱状图，按比例填充
  # geom_text(aes(label = scales::percent(percent, accuracy = 1)),
  #           position = position_fill(vjust = 0.5),
  #           size = 2.5, color = "white") +  # 白色文字提高对比度
  scale_fill_jco() +  # 使用JCO期刊风格的配色
  # scale_fill_npg()      # Nature Publishing Group 风格
  # scale_fill_aaas()     # JAMA 期刊配色方案
  # scale_fill_lancet()   # Lancet 期刊风格
  scale_y_continuous(labels = scales::percent) +  # Y轴显示百分比格式
  labs(x = NULL, y = "Proportion", fill = "Cancer Type") +  # 标题和标签
  coord_flip() +  # 水平翻转，使地区名称显示更清晰
  facet_grid(. ~ year) +  # 按年份分面
  theme_minimal(base_size = 12) +  # 使用极简主题
  theme(
    legend.position = "bottom",  # 图例位置在底部
    axis.text.y = element_text(size = 8),  # Y轴文本大小
    strip.text = element_text(face = "bold")  # 分面标题加粗
  )

# 保存图像
dir.create("outcome", recursive = TRUE, showWarnings = FALSE)
ggsave("outcome/percent.tiff", width = 12, height = 8, dpi = 300)
cat("成分比图已保存为 outcome/percent.tiff\n")
