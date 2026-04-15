################################################################################
# EAPC三线表脚本
# 功能：计算各疾病的EAPC数据，生成三线表，并导出到Excel文件
# 输入：original_data.Rdata, order.Rdata
# 输出：All_Disease_Results.Rdata, All_Diseases_Results.xlsx
################################################################################

# 相关包准备
#install.packages("openxlsx")  # 安装openxlsx包（如果未安装）

# 设置工作路径
setwd('~/Documents/学习相关/统计建模/统计建模大赛') 

# 加载必要的包
library(dplyr)                  # 用于数据处理
library(ggplot2)                # 用于数据可视化
library(openxlsx)               # 用于Excel文件操作

# 加载数据
cat("正在加载数据...\n")
load("data/processed/original_data.Rdata")
load("data/processed/order.Rdata")

# 合并地区顺序数据
order_all <- rbind(order, order_country)
order <- order_all

################################################################################
# EAPC_count函数
# 功能：计算EAPC数据
# 参数：
#   - data: 输入数据
#   - age: 年龄组ID，默认为24
#   - measure: 测量指标ID
#   - metric: 指标类型ID，默认为1
# 返回值：包含EAPC计算结果的列表
################################################################################
EAPC_count <- function(data, age = 24, measure, metric = 1) {
  EC <- data
  
  # 合并地区顺序数据
  EC <- EC %>%
    inner_join(order, by = "location_name")
  
  # 1990年数据
  EC_1990 <- subset(EC, 
                    EC$year == 1990 &
                    EC$age_id == age &
                    EC$metric_id == 1 &
                    EC$measure_id == measure
  )
  
  # 去重并选择需要的列
  EC_1990 <- EC_1990 %>% distinct(location_name, .keep_all = TRUE)
  EC_1990 <- EC_1990[, c(4, 8, 14, 15, 16)]  # 选择地区以及对应的数值
  
  # 四舍五入并格式化输出
  EC_1990$val <- round(EC_1990$val, 1)  # 四舍五入到1位小数
  EC_1990$lower <- round(EC_1990$lower, 1)  # 四舍五入到1位小数
  EC_1990$upper <- round(EC_1990$upper, 1)  # 四舍五入到1位小数
  EC_1990$Num_1990 <- paste(EC_1990$lower, EC_1990$upper, sep = '-')  # 组合上下限
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990, ')', sep = '')  # 添加右括号
  EC_1990$Num_1990 <- paste('(', EC_1990$Num_1990, sep = '')  # 添加左括号
  EC_1990$Num_1990 <- paste(EC_1990$val, EC_1990$Num_1990, sep = ' ')  # 组合数值和置信区间
  
  # 2021年数据
  EC_2021 <- subset(EC, 
                    EC$year == 2021 &
                    EC$age_id == age &
                    EC$metric_id == 1 &
                    EC$measure_id == measure
  )
  
  # 去重并选择需要的列
  EC_2021 <- EC_2021 %>% distinct(location_name, .keep_all = TRUE)
  EC_2021 <- EC_2021[, c(4, 8, 14, 15, 16)]  # 选择地区以及对应的数值
  
  # 四舍五入并格式化输出
  EC_2021$val <- round(EC_2021$val, 1)  # 四舍五入到1位小数
  EC_2021$lower <- round(EC_2021$lower, 1)  # 四舍五入到1位小数
  EC_2021$upper <- round(EC_2021$upper, 1)  # 四舍五入到1位小数
  EC_2021$Num_2021 <- paste(EC_2021$lower, EC_2021$upper, sep = '-')  # 组合上下限
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021, ')', sep = '')  # 添加右括号
  EC_2021$Num_2021 <- paste('(', EC_2021$Num_2021, sep = '')  # 添加左括号
  EC_2021$Num_2021 <- paste(EC_2021$val, EC_2021$Num_2021, sep = ' ')  # 组合数值和置信区间
  
  # 1990年ASR数据
  ASR_1990 <- subset(EC, 
                     EC$year == 1990 & 
                     EC$age_id == 27 & 
                     EC$metric_id == 3 &
                     EC$measure_id == measure)
  
  # 去重并选择需要的列
  ASR_1990 <- ASR_1990 %>% distinct(location_name, .keep_all = TRUE)
  ASR_1990 <- ASR_1990[, c(4, 14, 15, 16)]  # 选择地区以及对应的数值
  
  # 四舍五入并格式化输出
  ASR_1990$val <- round(ASR_1990$val, 1)  # 四舍五入到1位小数
  ASR_1990$lower <- round(ASR_1990$lower, 1)  # 四舍五入到1位小数
  ASR_1990$upper <- round(ASR_1990$upper, 1)  # 四舍五入到1位小数
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower, ASR_1990$upper, sep = '-')  # 组合上下限
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990, ')', sep = '')  # 添加右括号
  ASR_1990$ASR_1990 <- paste('(', ASR_1990$ASR_1990, sep = '')  # 添加左括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val, ASR_1990$ASR_1990, sep = ' ')  # 组合数值和置信区间
  
  # 2021年ASR数据
  ASR_2021 <- subset(EC, 
                     EC$year == 2021 & 
                     EC$age_name == 'Age-standardized' & 
                     EC$metric_id == 3 &
                     EC$measure_id == measure)
  
  # 去重并选择需要的列
  ASR_2021 <- ASR_2021 %>% distinct(location_name, .keep_all = TRUE)
  ASR_2021 <- ASR_2021[, c(4, 14, 15, 16)]  # 选择地区以及对应的数值
  
  # 四舍五入并格式化输出
  ASR_2021$val <- round(ASR_2021$val, 1)  # 四舍五入到1位小数
  ASR_2021$lower <- round(ASR_2021$lower, 1)  # 四舍五入到1位小数
  ASR_2021$upper <- round(ASR_2021$upper, 1)  # 四舍五入到1位小数
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower, ASR_2021$upper, sep = '-')  # 组合上下限
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021, ')', sep = '')  # 添加右括号
  ASR_2021$ASR_2021 <- paste('(', ASR_2021$ASR_2021, sep = '')  # 添加左括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val, ASR_2021$ASR_2021, sep = ' ')  # 组合数值和置信区间
  
  # 准备EAPC计算数据
  EAPC <- subset(EC, 
                 EC$age_name == 'Age-standardized' & 
                 EC$metric_name == 'Rate' &
                 EC$measure_id == measure)
  
  # 选择需要的列
  EAPC <- EAPC[, c(4, 13, 14)]  # 选择地区、年份和数值
  
  # 初始化EAPC计算结果数据框
  country <- ASR_1990$location_name
  EAPC_cal <- data.frame(location_name = country, 
                         EAPC = rep(0, times = length(country)), 
                         UCI = rep(0, times = length(country)), 
                         LCI = rep(0, times = length(country)))
  
  # 计算每个国家的EAPC
  for (i in 1:length(country)) {
    country_cal <- as.character(EAPC_cal[i, 1])  # 获取当前国家
    a <- subset(EAPC, EAPC$location_name == country_cal)  # 获取当前国家的数据
    a$y <- log(a$val)  # 计算对数值
    mod_simp_reg <- lm(y ~ year, data = a)  # 拟合线性模型
    
    # 计算EAPC及其置信区间
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100  # 计算EAPC
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算下限
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100  # 计算上限
    
    # 保存结果
    EAPC_cal[i, 2] <- estimate
    EAPC_cal[i, 4] <- low
    EAPC_cal[i, 3] <- high
  }
  
  # 四舍五入并格式化输出
  EAPC_cal$EAPC <- round(EAPC_cal$EAPC, 2)  # 四舍五入到2位小数
  EAPC_cal$UCI <- round(EAPC_cal$UCI, 2)  # 四舍五入到2位小数
  EAPC_cal$LCI <- round(EAPC_cal$LCI, 2)  # 四舍五入到2位小数
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI, EAPC_cal$UCI, sep = '-')  # 组合上下限
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI, ')', sep = '')  # 添加右括号
  EAPC_cal$EAPC_CI <- paste('(', EAPC_cal$EAPC_CI, sep = '')  # 添加左括号
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC, EAPC_cal$EAPC_CI, sep = ' ')  # 组合数值和置信区间
  
  # 准备结果表
  EC_1990_table <- EC_1990[, c(1, 6)]  # 选择地区和1990年数值
  ASR_1990_table <- ASR_1990[, c(1, 5)]  # 选择地区和1990年ASR
  EC_2021_table <- EC_2021[, c(1, 6)]  # 选择地区和2021年数值
  ASR_2021_tabel <- ASR_2021[, c(1, 5)]  # 选择地区和2021年ASR
  EAPC_cal_talbe <- EAPC_cal[, c(1, 5)]  # 选择地区和EAPC
  
  # 合并结果
  Results_table <- merge(EC_1990_table, ASR_1990_table, by = 'location_name')
  Results_table <- merge(Results_table, EC_2021_table, by = 'location_name')
  Results_table <- merge(Results_table, ASR_2021_tabel, by = 'location_name')
  Results_table <- merge(Results_table, EAPC_cal_talbe, by = 'location_name')
  
  # 构建返回结果
  Results <- list(
    table = Results_table,  # 结果表
    EC_1990 = EC_1990,  # 1990年数据
    ASR_1990 = ASR_1990,  # 1990年ASR数据
    EC_2021 = EC_2021,  # 2021年数据
    ASR_2021 = ASR_2021,  # 2021年ASR数据
    EAPC = EAPC_cal  # EAPC数据
  )
  
  return(Results)
}

# 处理胃癌数据
cat("正在处理胃癌数据...\n")
stomach_Deaths <- EAPC_count(data = stomach_data, measure = 1)
stomach_DALYS <- EAPC_count(data = stomach_data, measure = 2)
stomach_Incidence <- EAPC_count(data = stomach_data, measure = 6)
stomach <- list(Death = stomach_Deaths,
                DALYS = stomach_DALYS,
                Incidence = stomach_Incidence)

# 处理肝癌数据
cat("正在处理肝癌数据...\n")
liver_Deaths <- EAPC_count(data = liver_data, measure = 1)
liver_DALYS <- EAPC_count(data = liver_data, measure = 2)
liver_Incidence <- EAPC_count(data = liver_data, measure = 6)
liver <- list(Death = liver_Deaths,
                DALYS = liver_DALYS,
                Incidence = liver_Incidence)

# 处理口腔癌数据
cat("正在处理口腔癌数据...\n")
cavity_Deaths <- EAPC_count(data = cavity_data, measure = 1)
cavity_DALYS <- EAPC_count(data = cavity_data, measure = 2)
cavity_Incidence <- EAPC_count(data = cavity_data, measure = 6)
cavity <- list(Death = cavity_Deaths,
                DALYS = cavity_DALYS,
                Incidence = cavity_Incidence)

# 处理结直肠癌数据
cat("正在处理结直肠癌数据...\n")
colon_Deaths <- EAPC_count(data = colon_data, measure = 1)
colon_DALYS <- EAPC_count(data = colon_data, measure = 2)
colon_Incidence <- EAPC_count(data = colon_data, measure = 6)
colon <- list(Death = colon_Deaths,
                DALYS = colon_DALYS,
                Incidence = colon_Incidence)

# 处理胰腺癌数据
cat("正在处理胰腺癌数据...\n")
pancreatic_Deaths <- EAPC_count(data = pancreatic_data, measure = 1)
pancreatic_DALYS <- EAPC_count(data = pancreatic_data, measure = 2)
pancreatic_Incidence <- EAPC_count(data = pancreatic_data, measure = 6)
pancreatic <- list(Death = pancreatic_Deaths,
                DALYS = pancreatic_DALYS,
                Incidence = pancreatic_Incidence)

# 保存结果
cat("正在保存结果...\n")
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
save(stomach, colon, liver, pancreatic, cavity, file = "data/processed/All_Disease_Results.Rdata")

################################################################################
# process_disease函数
# 功能：处理疾病数据，生成合并的结果表
# 参数：
#   - disease_data: 疾病数据
#   - disease_name: 疾病名称
# 返回值：合并后的结果表
################################################################################
process_disease <- function(disease_data, disease_name) {
  # 生成三个指标的数据框
  deaths <- EAPC_count(data = disease_data, measure = 1)[["table"]]  # 死亡率
  dalys <- EAPC_count(data = disease_data, measure = 2)[["table"]]  # DALYs
  incidence <- EAPC_count(data = disease_data, measure = 6)[["table"]]  # 发病率
  
  # 按列名合并三个指标（假设 location_name 顺序一致）
  combined <- cbind(
    deaths,
    dalys[, -1],  # 排除重复的 location_name 列
    incidence[, -1]  # 排除重复的 location_name 列
  )
  
  # 重命名列以区分指标
  colnames(combined) <- c(
    "location_name",
    paste0("Deaths_", colnames(deaths)[-1]),  # 死亡率列
    paste0("DALYs_", colnames(dalys)[-1]),  # DALYs列
    paste0("Incidence_", colnames(incidence)[-1])  # 发病率列
  )
  
  return(combined)
}

# 创建 Excel 工作簿
cat("正在创建Excel工作簿...\n")
wb <- createWorkbook()

# 定义疾病列表
diseases <- list(
  stomach = stomach_data,      # 胃癌
  liver = liver_data,          # 肝癌
  cavity = cavity_data,        # 口腔癌
  colon = colon_data,          # 结直肠癌
  pancreatic = pancreatic_data  # 胰腺癌
)

# 循环处理每个疾病
for (disease_name in names(diseases)) {
  cat(paste("正在处理", disease_name, "数据...\n"))
  # 处理数据
  df <- process_disease(diseases[[disease_name]], disease_name)
  
  # 添加工作表并写入数据
  addWorksheet(wb, disease_name)
  writeData(wb, sheet = disease_name, x = df)
}

# 保存 Excel 文件
cat("正在保存Excel文件...\n")
saveWorkbook(wb, "All_Diseases_Results.xlsx", overwrite = TRUE)

cat("数据处理完成！\n")



