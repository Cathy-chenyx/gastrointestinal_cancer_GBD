################################################################################
# SDI与发病率的相关图脚本
# 功能：分析社会人口指数（SDI）与癌症发病率的关系，生成散点图和趋势线
# 输入：合并后的疾病数据和SDI数据
# 输出：SDI与ASR的相关图，保存在outcome目录中
################################################################################

#####相关包加载以及数据导入
setwd('..')  # 设置工作路径为项目根目录

# 加载必要的包
library(reshape)      # 用于数据重塑
library(ggplot2)      # 用于数据可视化
library(ggrepel)      # 用于防止标签重叠
library(dplyr)        # 用于数据处理
library(readr)        # 用于读取CSV文件
library(cowplot)      # 用于提取图例
library(patchwork)    # 用于图形组合
library(gridExtra)    # 用于组合图形

# 数据导入与整合 -----------------------------------------------------------------

# 加载合并后的数据
cat("正在加载数据...\n")
load("data/processed/original_data.Rdata")  # 加载合并后的疾病数据
load("data/processed/order.Rdata")  # 加载国家排序数据

# 数据整合
cat("正在整合数据...\n")
merge_data <- bind_rows(
  stomach_data, liver_data, colon_data, pancreatic_data, cavity_data,
  .id = "disease"  # 自动添加疾病来源标识
) %>%
  mutate(
    # 重命名疾病标识
    disease = case_when(
      disease == "1" ~ "Stomach Cancer",  # 胃癌
      disease == "2" ~ "Liver Cancer",    # 肝癌
      disease == "3" ~ "Colon Cancer",    # 结直肠癌
      disease == "4" ~ "Pancreatic Cancer",  # 胰腺癌
      disease == "5" ~ "Cavity Cancer"     # 口腔癌
    ),
    # 转换年龄组为有序因子
    age = factor(age_name, 
                 # levels = c("15-19years", "20-24years", "25-29years",
                 #    "30-34years", "35-39years","40-45years","Age-standardized"),
                 ordered = TRUE)
  ) %>%
  filter( # 选择目标数据
    metric_name == "Rate" ,     # 使用标准化率
  ) 

cat("数据整合完成，共", nrow(merge_data), "行数据\n")

# SDI_ASR图函数 ----------------------------------------------------------------

################################################################################
# generate_SDI_ASR函数
# 功能：生成SDI与ASR（年龄标准化率）的相关图
# 参数：
#   - data: 输入数据
#   - name: 输出文件名前缀
# 返回值：包含Incidence和DALYS图形对象的列表
################################################################################
generate_SDI_ASR <- function(data, name){
  # 选择需要的列
  data <- data[,c(2,4,6,8,10,12,13,14)]
  
  # 读取SDI排序和SDI数据
  order_SDI <- read.csv('data/processed/order_SDI.csv', header = F)  # 读取SDI排序文件
  SDI <- read_csv("data/processed/SDI_clean.csv")  # 读取SDI数据
  SDI <- SDI[,-1]  # 移除第一列
  
  # 重命名SDI数据列名
  names(SDI) <- c('location', 'year', 'SDI')
  
  ### 获取1990-2021的标化发病率数据
  EC <- subset(data, data$age_name == 'Age-standardized' & 
                 data$metric_name == 'Rate' &
                 data$measure_name == 'Incidence')  # 筛选发病率数据
  
  # 选择需要的列并重命名
  EC <- EC[,c(2,7,8)]
  names(EC)[3] <- 'ASR'  # 重命名为ASR（年龄标准化率）
  names(EC)[1] <- 'location'  # 重命名为location
  
  ### 合并SDI和ASR数据
  EC_ASR_SDI <- merge(EC, SDI, by = c('location', 'year'))  # 按location和year合并
  
  # 设置location的顺序
  EC_ASR_SDI$location <- factor(EC_ASR_SDI$location, 
                                levels = order_SDI$V1, 
                                ordered = TRUE)  # 按照SDI排序文件设置顺序
  
  # 移除NA值
  EC_ASR_SDI <- na.omit(EC_ASR_SDI)
  
  ### 开始作图 主变量为SDI和ASR
  Incidence <- ggplot(EC_ASR_SDI, aes(SDI, ASR)) + 
    geom_point(aes(color = location, shape = location)) +  # 绘制散点图，按location着色和形状
    scale_shape_manual(values = 1:22) +  # 设置形状
    geom_smooth(colour = 'black', method = 'loess', se = FALSE, span = 0.5) +  # 添加趋势线
    labs(
      x = "SDI in 2021",  # x轴标签
      y = "ASR of incidence per 100000 population"  # y轴标签
    ) +
    theme_minimal() +  # 使用简约主题
    theme(legend.position = "none")  # 不显示图例
  
  ### 获取1990-2021的标化DALYs数据
  EC <- subset(data, data$age_name == 'Age-standardized' & 
                 data$metric_name == 'Rate' &
                 data$measure_name == 'DALYs (Disability-Adjusted Life Years)')  # 筛选DALYs数据
  
  # 选择需要的列并重命名
  EC <- EC[,c(2,7,8)]
  names(EC)[3] <- 'ASR'  # 重命名为ASR（年龄标准化率）
  names(EC)[1] <- 'location'  # 重命名为location
  
  ### 合并SDI和ASR数据
  EC_ASR_SDI <- merge(EC, SDI, by = c('location', 'year'))  # 按location和year合并
  
  # 设置location的顺序
  EC_ASR_SDI$location <- factor(EC_ASR_SDI$location, 
                                levels = order_SDI$V1, 
                                ordered = TRUE)  # 按照SDI排序文件设置顺序
  
  # 移除NA值
  EC_ASR_SDI <- na.omit(EC_ASR_SDI)
  
  ### 开始作图 主变量为SDI和ASR
  DALYS <- ggplot(EC_ASR_SDI, aes(SDI, ASR)) + 
    geom_point(aes(color = location, shape = location)) +  # 绘制散点图，按location着色和形状
    scale_shape_manual(values = 1:22) +  # 设置形状
    geom_smooth(colour = 'black', method = 'loess', se = FALSE, span = 0.5) +  # 添加趋势线
    labs(
      x = "SDI in 2021",  # x轴标签
      y = "ASR of DALYS per 100000 population"  # y轴标签
    ) +
    theme_minimal() +  # 使用简约主题
    theme(legend.position = "none")  # 不显示图例
  
  # 保存图形（可选）
  # ggsave(filename = paste0(name, "_Incidence.png"), Incidence, width = 6, height = 8)
  # ggsave(filename = paste0(name, "_DALYS.png"), DALYS, width = 6, height = 8)
  
  # 返回Incidence和DALYS图形对象以及处理后的数据
  return(list(Incidence = Incidence, DALYS = DALYS, EC_ASR_SDI = EC_ASR_SDI))
}

# 绘制图例函数 --------------------------------------------------------------------

################################################################################
# generate_legend函数
# 功能：生成共享图例
# 参数：
#   - data: 输入数据
#   - name: 输出文件名前缀
# 返回值：图例对象
################################################################################
generate_legend <- function(data, name){
  # 选择需要的列
  data <- data[,c(2,4,6,8,10,12,13,14)]
  
  # 读取SDI排序和SDI数据
  order_SDI <- read.csv('data/processed/order_SDI.csv', header = F)  # 读取SDI排序文件
  SDI <- read_csv("data/processed/SDI_clean.csv")  # 读取SDI数据
  SDI <- SDI[,-1]  # 移除第一列
  
  # 重命名SDI数据列名
  names(SDI) <- c('location', 'year', 'SDI')
  
  ### 获取1990-2021的标化发病率数据
  EC <- subset(data, data$age_name == 'Age-standardized' & 
                 data$metric_name == 'Rate' &
                 data$measure_name == 'Incidence')  # 筛选发病率数据
  
  # 选择需要的列并重命名
  EC <- EC[,c(2,7,8)]
  names(EC)[3] <- 'ASR'  # 重命名为ASR（年龄标准化率）
  names(EC)[1] <- 'location'  # 重命名为location
  
  ### 合并SDI和ASR数据
  EC_ASR_SDI <- merge(EC, SDI, by = c('location', 'year'))  # 按location和year合并
  
  # 设置location的顺序
  EC_ASR_SDI$location <- factor(EC_ASR_SDI$location, 
                                levels = order_SDI$V1, 
                                ordered = TRUE)  # 按照SDI排序文件设置顺序
  
  # 移除NA值
  EC_ASR_SDI <- na.omit(EC_ASR_SDI)
  
  # 准备图例数据
  legend_data <- data.frame(
    location = unique(EC_ASR_SDI$location)  # 使用所有可能的地点
  )
  
  # 创建图例的基础图形
  legend_plot <- ggplot(legend_data, aes(x = location, y = 0, color = location, shape = location)) +
    geom_point() +  # 只需点图即可
    scale_shape_manual(values = 1:22) +  # 使用与主图相同的形状
    guides(
      color = guide_legend(nrow = 4, ncol = 6),  # 设置图例为4行6列
      shape = guide_legend(nrow = 4, ncol = 6)   # 设置图例为4行6列
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),  # 去掉轴标题
      axis.text = element_blank(),    # 去掉轴文本
      axis.ticks = element_blank(),   # 去掉轴刻度
      panel.grid = element_blank(),   # 去掉网格线
      legend.position = "right",      # 设置图例位置
      legend.title = element_blank(), # 去掉图例标题
      legend.text = element_text(size = 8)  # 调整图例文字大小
    )
  
  # 提取图例
  legend <- get_legend(legend_plot)
  return(legend)
}

# 生成并保存图例
cat("正在生成图例...\n")
legend <- generate_legend(colon_data, "colon")
# 保存图例为单独的文件
ggsave(filename = "outcome/shared_legend.png", plot = legend, width = 12, height = 6)
cat("图例已保存到 outcome/shared_legend.png\n")

# 组合图形 --------------------------------------------------------------------

# 调用函数并收集图形对象
cat("正在生成各类疾病的SDI与ASR相关图...\n")
colon_plots <- generate_SDI_ASR(colon_data, "colon")  # 结直肠癌
stomach_plots <- generate_SDI_ASR(stomach_data, "stomach")  # 胃癌
pancreatic_plots <- generate_SDI_ASR(pancreatic_data, "pancreatic")  # 胰腺癌
cavity_plots <- generate_SDI_ASR(cavity_data, "cavity")  # 口腔癌
liver_plots <- generate_SDI_ASR(liver_data, "liver")  # 肝癌

# 提取所有Incidence和DALYS图
incidence_plots <- list(
  colon_plots$Incidence,    # 结直肠癌发病率
  stomach_plots$Incidence,  # 胃癌发病率
  pancreatic_plots$Incidence,  # 胰腺癌发病率
  cavity_plots$Incidence,    # 口腔癌发病率
  liver_plots$Incidence      # 肝癌发病率
)

dalys_plots <- list(
  colon_plots$DALYS,         # 结直肠癌DALYs
  stomach_plots$DALYS,       # 胃癌DALYs
  pancreatic_plots$DALYS,    # 胰腺癌DALYs
  cavity_plots$DALYS,        # 口腔癌DALYs
  liver_plots$DALYS          # 肝癌DALYs
)

# 保存SDI数据
SDI_data <- list(
  colon = colon_plots$EC_ASR_SDI,
  stomach = stomach_plots$EC_ASR_SDI,
  pancreatic = pancreatic_plots$EC_ASR_SDI,
  cavity = cavity_plots$EC_ASR_SDI,
  liver = liver_plots$EC_ASR_SDI
)

# 显示图例
grid::grid.draw(legend)

# 创建 Incidence 和 DALYS 的布局
cat("正在组合图形...\n")
incidence_row <- grid.arrange(incidence_plots[[1]], incidence_plots[[2]], incidence_plots[[3]], incidence_plots[[4]], incidence_plots[[5]], ncol = 5)
dalys_row <- grid.arrange(dalys_plots[[1]], dalys_plots[[2]], dalys_plots[[3]], dalys_plots[[4]], dalys_plots[[5]], ncol = 5)

# 将两行图垂直排列
combined_plot <- arrangeGrob(incidence_row, dalys_row, ncol = 1, heights = c(1, 1))
combined_plot <- arrangeGrob(combined_plot, legend, ncol = 1, heights = c(3, 1)) 

# 显示图形
grid::grid.draw(combined_plot)

# 保存图形
cat("正在保存图形...\n")
ggsave("outcome/combined_plot.png", plot = combined_plot, width = 16, height = 10, dpi = 300)
cat("图形已保存到 outcome/combined_plot.png\n")

# 添加标签 --------------------------------------------------------------------
# 生成所有图形并添加标签 ------------------------------------------------------------

# 调用函数并收集图形对象
cat("正在生成带标签的图形...\n")
colon_plots <- generate_SDI_ASR(colon_data, "colon")
stomach_plots <- generate_SDI_ASR(stomach_data, "stomach")
pancreatic_plots <- generate_SDI_ASR(pancreatic_data, "pancreatic")
cavity_plots <- generate_SDI_ASR(cavity_data, "cavity")
liver_plots <- generate_SDI_ASR(liver_data, "liver")

# 提取所有图形并添加标签
all_plots <- list(
  colon_plots$Incidence,    # A: 结直肠癌发病率
  stomach_plots$Incidence,  # B: 胃癌发病率
  pancreatic_plots$Incidence,  # C: 胰腺癌发病率
  cavity_plots$Incidence,    # D: 口腔癌发病率
  liver_plots$Incidence,     # E: 肝癌发病率
  colon_plots$DALYS,         # F: 结直肠癌DALYs
  stomach_plots$DALYS,       # G: 胃癌DALYs
  pancreatic_plots$DALYS,    # H: 胰腺癌DALYs
  cavity_plots$DALYS,        # I: 口腔癌DALYs
  liver_plots$DALYS          # J: 肝癌DALYs
)

# 为每个图形添加A-J的标签
for (i in 1:10) {
  all_plots[[i]] <- all_plots[[i]] +
    labs(tag = LETTERS[i]) +  # 使用大写字母A-J
    theme(
      plot.tag.position = c(0.05, 0.95),  # 左上角位置（相对坐标）
      plot.tag = element_text(
        size = 12, 
        face = "bold", 
        color = "black"
      )
    )
}

# 组合图形 --------------------------------------------------------------------

# 重新组织图形对象
incidence_row <- grid.arrange(grobs = all_plots[1:5], ncol = 5)  # 第一行A-E：发病率
 dalys_row <- grid.arrange(grobs = all_plots[6:10], ncol = 5)     # 第二行F-J：DALYs

# 合并行
combined_plot <- arrangeGrob(
  incidence_row, 
  dalys_row, 
  ncol = 1, 
  heights = c(1, 1)
) 

# 最终输出（含图例）
ggsave(
  "outcome/combined_plot_labeled.png", 
  plot = combined_plot, 
  width = 18, 
  height = 8, 
  dpi = 300
)

cat("带标签的图形已保存到 outcome/combined_plot_labeled.png\n")
cat("所有图形生成完成！\n")


