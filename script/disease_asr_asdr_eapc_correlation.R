################################################################################
# 疾病ASR、ASDR和EAPC相关性分析图.R脚本
# 功能：分析癌症的ASR、ASDR与EAPC之间的相关性，以及HDI与EAPC之间的相关性
# 输入：colon_data（结直肠癌数据）、HDI.csv（人类发展指数数据）
# 输出：相关性分析图
################################################################################

# 设置工作路径
setwd('/Users/cathy/Documents/学习相关/统计建模/统计建模大赛')

# 加载必要的包
library(dplyr)  # 用于数据处理
library(ggplot2)  # 用于数据可视化

# 加载数据
# EC <- read.csv('EC_nation.csv', header = T)  # 读取国家数据
EC <- colon_data  # 使用结直肠癌数据

# 1990年ASIR与EAPC相关性分析 -------------------------------------------------
cat("正在分析1990年ASIR与EAPC相关性...\n")

# 获取1990年年龄标准化发病率（ASIR）
ASIR_1990 <- subset(EC, EC$year == 1990 & 
                      EC$age_name == 'Age-standardized' & 
                      EC$metric_name == 'Rate' &
                      EC$measure_name == 'Incidence')  # 1990年年龄校正后的发病率
ASIR_1990 <- ASIR_1990[, c(4, 14)]  # 选择地区名称和数值列
names(ASIR_1990)[2] <- 'ASR'  # 重命名为ASR

# 获取1990年绝对发病数
Incidence_case_1990 <- subset(EC, EC$year == 1990 & 
                                EC$age_id == '24' & 
                                EC$metric_name == 'Number' &
                                EC$measure_name == 'Incidence')
Incidence_case_1990 <- Incidence_case_1990[, c(4, 14)]  # 选择地区名称和数值列
names(Incidence_case_1990)[2] <- 'case'  # 重命名为case

# 计算EAPC
EAPC <- subset(EC, EC$age_name == 'Age-standardized' & 
                 EC$metric_name == 'Rate' &
                 EC$measure_name == 'Incidence')

EAPC <- EAPC[, c(4, 13, 14)]  # 选择地区名称、年份和数值列

country <- ASIR_1990$location_name  # 获取国家名称
EAPC_cal <- data.frame(location_name = country, 
                       EAPC = rep(0, times = length(country)),
                       UCI = rep(0, times = length(country)),
                       LCI = rep(0, times = length(country)))

# 循环计算每个国家的EAPC
for (i in 1:length(country)) {
  country_cal <- as.character(EAPC_cal[i, 1])  # 获取当前国家名称
  a <- subset(EAPC, EAPC$location == country_cal)  # 提取当前国家的数据
  a$y <- log(a$val)  # 对数转换
  mod_simp_reg <- lm(y ~ year, data = a)  # 线性回归模型
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100  # 计算EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算下限
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算上限
  EAPC_cal[i, 2] <- estimate  # 保存EAPC值
  EAPC_cal[i, 4] <- low  # 保存下限
  EAPC_cal[i, 3] <- high  # 保存上限
}

EAPC_cal <- EAPC_cal[, c(1, 2)]  # 只保留地区名称和EAPC列

# 合并三者成为一个数据集
Total <- merge(Incidence_case_1990, EAPC_cal, by = 'location_name')  # 合并发病数和EAPC
Total <- merge(Total, ASIR_1990, by = 'location_name')  # 合并ASIR
Total_incidence <- Total  # 保存发病率数据
Total_incidence$group <- 'ASIR'  # 指示变量提示该数据为发病率数据

# 1990年ASDR与EAPC相关性分析 -------------------------------------------------
cat("正在分析1990年ASDR与EAPC相关性...\n")

# 获取1990年年龄标准化死亡率（ASDR）
ASDR_1990 <- subset(EC, EC$year == 1990 & 
                      EC$age_name == 'Age-standardized' & 
                      EC$metric_name == 'Rate' &
                      EC$measure_name == 'Deaths')  # 1990年年龄校正后的死亡率
ASDR_1990 <- ASDR_1990[, c(4, 14)]  # 选择地区名称和数值列
names(ASDR_1990)[2] <- 'ASR'  # 重命名为ASR

# 获取1990年绝对死亡数
Deaths_case_1990 <- subset(EC, EC$year == 1990 & 
                             EC$age_id == '24' & 
                             EC$metric_name == 'Number' &
                             EC$measure_name == 'Deaths')
Deaths_case_1990 <- Deaths_case_1990[, c(4, 14)]  # 选择地区名称和数值列
names(Deaths_case_1990)[2] <- 'case'  # 重命名为case

# 计算EAPC
EAPC <- subset(EC, EC$age_name == 'Age-standardized' & 
                 EC$metric_name == 'Rate' &
                 EC$measure_name == 'Deaths')

EAPC <- EAPC[, c(4, 13, 14)]  # 选择地区名称、年份和数值列

country <- ASDR_1990$location_name  # 获取国家名称
EAPC_cal <- data.frame(location_name = country, 
                       EAPC = rep(0, times = length(country)),
                       UCI = rep(0, times = length(country)),
                       LCI = rep(0, times = length(country)))

# 循环计算每个国家的EAPC
for (i in 1:length(country)) {
  country_cal <- as.character(EAPC_cal[i, 1])  # 获取当前国家名称
  a <- subset(EAPC, EAPC$location == country_cal)  # 提取当前国家的数据
  a$y <- log(a$val)  # 对数转换
  mod_simp_reg <- lm(y ~ year, data = a)  # 线性回归模型
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100  # 计算EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算下限
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算上限
  EAPC_cal[i, 2] <- estimate  # 保存EAPC值
  EAPC_cal[i, 4] <- low  # 保存下限
  EAPC_cal[i, 3] <- high  # 保存上限
}

EAPC_cal <- EAPC_cal[, c(1, 2)]  # 只保留地区名称和EAPC列

# 合并数据
Total <- merge(Deaths_case_1990, EAPC_cal, by = 'location_name')  # 合并死亡数和EAPC
Total <- merge(Total, ASDR_1990, by = 'location_name')  # 合并ASDR
Total_Deaths <- Total  # 保存死亡率数据
Total_Deaths$group <- 'ASDR'  # 指示变量提示该为死亡率数据

# 合并发病率及死亡率数据
Total <- rbind(Total_incidence, Total_Deaths)

# 绘制图像
cat("正在绘制ASR与EAPC相关性图...\n")
Total$group <- factor(Total$group, 
                      levels = c('ASIR', 'ASDR'), 
                      ordered = TRUE)

p1 <- ggplot(Total, aes(ASR, EAPC, size = case)) +  
  geom_point(color = '#0099CC') +  # 绘制散点图
  geom_smooth(data = Total, aes(ASR, EAPC), se = .8, colour = 'black', span = 1) +  # 添加趋势线
  scale_size(name = 'Cases in 1990', breaks = c(100, 1000, 10000, 50000),
             labels = c("<500", "500-1,000", "10,000-50,000",
                        '>50,000')) +  # 设置点大小图例
  facet_grid(. ~ group, scales = "free") +  # 按group分面
  theme_light()  # 使用浅色主题

p1  # 显示图形

# 计算pearson相关系数及对应P值
cat("正在计算相关系数...\n")
print("发病率EAPC与ASR的相关系数：")
print(cor.test(Total_incidence$EAPC, Total_incidence$ASR, method = "pearson"))
print("死亡率EAPC与ASR的相关系数：")
print(cor.test(Total_Deaths$EAPC, Total_Deaths$ASR, method = "pearson"))

# 图D: HDI与EAPC相关性分析 ---------------------------------------------------
cat("正在分析HDI与EAPC相关性...\n")

# 读取HDI数据
HDI <- read.csv('GBD代码/GBD_COR/HDI.csv', header = T)
names(HDI) <- c('location_name', 'HDI')  # 重命名列

# 获取2019年绝对发病数
Incidence_case_2019 <- subset(EC, EC$year == 2019 & 
                                EC$age_id == 24 & 
                                EC$metric_name == 'Number' &
                                EC$measure_name == 'Incidence')
Incidence_case_2019 <- Incidence_case_2019[, c(4, 14)]  # 选择地区名称和数值列
names(Incidence_case_2019)[2] <- 'case'  # 重命名为case

# 计算EAPC
EAPC <- subset(EC, EC$age_name == 'Age-standardized' & 
                 EC$metric_name == 'Rate' &
                 EC$measure_name == 'Incidence')

EAPC <- EAPC[, c(4, 13, 14)]  # 选择地区名称、年份和数值列

country <- Incidence_case_2019$location_name  # 获取国家名称
EAPC_cal <- data.frame(location_name = country, 
                       EAPC = rep(0, times = length(country)),
                       UCI = rep(0, times = length(country)),
                       LCI = rep(0, times = length(country)))

# 循环计算每个国家的EAPC
for (i in 1:length(country)) {
  country_cal <- as.character(EAPC_cal[i, 1])  # 获取当前国家名称
  a <- subset(EAPC, EAPC$location_name == country_cal)  # 提取当前国家的数据
  a$y <- log(a$val)  # 对数转换
  mod_simp_reg <- lm(y ~ year, data = a)  # 线性回归模型
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100  # 计算EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算下限
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算上限
  EAPC_cal[i, 2] <- estimate  # 保存EAPC值
  EAPC_cal[i, 4] <- low  # 保存下限
  EAPC_cal[i, 3] <- high  # 保存上限
}

EAPC_cal <- EAPC_cal[, c(1, 2)]  # 只保留地区名称和EAPC列

# 合并三者数据
Total <- merge(Incidence_case_2019, EAPC_cal, by = 'location_name')  # 合并发病数和EAPC
Total <- merge(Total, HDI, by = 'location_name')  # 合并HDI
Total_incidence <- Total  # 保存发病率数据
Total_incidence$group <- 'ASIR'  # 指示变量

# 获取2019年绝对死亡数
Deaths_case_2019 <- subset(EC, EC$year == 2019 & 
                             EC$age_id == 24 & 
                             EC$metric_name == 'Number' &
                             EC$measure_name == 'Deaths')
Deaths_case_2019 <- Deaths_case_2019[, c(4, 14)]  # 选择地区名称和数值列
names(Deaths_case_2019)[2] <- 'case'  # 重命名为case

# 计算EAPC
EAPC <- subset(EC, EC$age_name == 'Age-standardized' & 
                 EC$metric_name == 'Rate' &
                 EC$measure_name == 'Deaths')

EAPC <- EAPC[, c(4, 13, 14)]  # 选择地区名称、年份和数值列

country <- Deaths_case_2019$location_name  # 获取国家名称
EAPC_cal <- data.frame(location_name = country, 
                       EAPC = rep(0, times = length(country)),
                       UCI = rep(0, times = length(country)),
                       LCI = rep(0, times = length(country)))

# 循环计算每个国家的EAPC
for (i in 1:length(country)) {
  country_cal <- as.character(EAPC_cal[i, 1])  # 获取当前国家名称
  a <- subset(EAPC, EAPC$location_name == country_cal)  # 提取当前国家的数据
  a$y <- log(a$val)  # 对数转换
  mod_simp_reg <- lm(y ~ year, data = a)  # 线性回归模型
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100  # 计算EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算下限
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算上限
  EAPC_cal[i, 2] <- estimate  # 保存EAPC值
  EAPC_cal[i, 4] <- low  # 保存下限
  EAPC_cal[i, 3] <- high  # 保存上限
}

EAPC_cal <- EAPC_cal[, c(1, 2)]  # 只保留地区名称和EAPC列

# 合并数据
Total <- merge(Deaths_case_2019, EAPC_cal, by = 'location_name')  # 合并死亡数和EAPC
Total <- merge(Total, HDI, by = 'location_name')  # 合并HDI
Total_Deaths <- Total  # 保存死亡率数据
Total_Deaths$group <- 'ASDR'  # 指示变量

# 合并发病率及死亡率数据
Total <- rbind(Total_incidence, Total_Deaths)

# 绘制图像
cat("正在绘制HDI与EAPC相关性图...\n")
Total$group <- factor(Total$group, 
                      levels = c('ASIR', 'ASDR'), 
                      ordered = TRUE)

p1 <- ggplot(Total, aes(HDI, EAPC, size = case)) +  
  geom_point(color = '#0099CC') +  # 绘制散点图
  geom_smooth(data = Total, aes(HDI, EAPC), se = .8, colour = 'black', span = 1) +  # 添加趋势线
  scale_size(name = 'Cases in 1990', breaks = c(100, 1000, 10000, 50000),
             labels = c("<500", "500-1,000", "10,000-50,000",
                        '>50,000')) +  # 设置点大小图例
  facet_grid(. ~ group, scales = "free") +  # 按group分面
  theme_light()  # 使用浅色主题

p1  # 显示图形

# 计算pearson相关系数
cat("正在计算HDI与EAPC的相关系数...\n")
print("发病率EAPC与HDI的相关系数：")
print(cor.test(Total_incidence$EAPC, Total_incidence$HDI, method = "pearson"))
print("死亡率EAPC与HDI的相关系数：")
print(cor.test(Total_Deaths$EAPC, Total_Deaths$HDI, method = "pearson"))

cat("分析完成！\n")

