################################################################################
# EAPC计算函数脚本
# 功能：计算癌症指标的年度百分比变化（EAPC），生成包含1990年和2021年数据的对比表格
# 输入：EC数据框（需在调用前加载）
# 输出：包含1990年和2021年数据及EAPC的结果表格
################################################################################

################################################################################
# EAPC_count函数
# 功能：计算EAPC并生成结果表格
# 参数：
#   - age: 年龄组ID，默认为8
#   - measure: 指标ID（1=死亡率，2=DALYs，6=发病率）
#   - metric: 度量标准ID，默认为1
# 返回值：包含1990年和2021年数据及EAPC的结果表格
################################################################################
EAPC_count <- function(age=8, measure, metric = 1){
  # 提取1990年数据
  EC_1990 <- subset(EC, 
                    EC$year == 1990 &
                    EC$age_id == age &
                    EC$metric_id == 1 &
                    EC$measure_id == measure
  )
  
  # 去重，保留每个地区的唯一记录
  EC_1990 <- EC_1990 %>% distinct(location_name, .keep_all = TRUE)
  
  # 选择需要的列：地区名称、年龄组名称、数值、下限、上限
  EC_1990 <- EC_1990[, c(4, 8, 14, 15, 16)]
  
  # 四舍五入数值
  EC_1990$val <- round(EC_1990$val, 1)  # 四舍五入到小数点后1位
  EC_1990$lower <- round(EC_1990$lower, 1)  # 四舍五入到小数点后1位
  EC_1990$upper <- round(EC_1990$upper, 1)  # 四舍五入到小数点后1位
  
  # 构建95%置信区间的字符串表示
  EC_1990$Num_1990 <- paste(EC_1990$lower, EC_1990$upper, sep = '-')  # 格式：lower-upper
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990, ')', sep = '')  # 添加右括号
  EC_1990$Num_1990 <- paste('(', EC_1990$Num_1990, sep = '')  # 添加左括号
  EC_1990$Num_1990 <- paste(EC_1990$val, EC_1990$Num_1990, sep = ' ')  # 格式：val (lower-upper)
  
  
  # 提取2021年数据（注意：原代码中错误地使用了1990年，这里已修正）
  EC_2021 <- subset(EC, 
                    EC$year == 2021 &
                    EC$age_id == age &
                    EC$metric_id == 1 &
                    EC$measure_id == measure
  )
  
  # 去重，保留每个地区的唯一记录
  EC_2021 <- EC_2021 %>% distinct(location_name, .keep_all = TRUE)
  
  # 选择需要的列：地区名称、年龄组名称、数值、下限、上限
  EC_2021 <- EC_2021[, c(4, 8, 14, 15, 16)]
  
  # 四舍五入数值
  EC_2021$val <- round(EC_2021$val, 1)  # 四舍五入到小数点后1位
  EC_2021$lower <- round(EC_2021$lower, 1)  # 四舍五入到小数点后1位
  EC_2021$upper <- round(EC_2021$upper, 1)  # 四舍五入到小数点后1位
  
  # 构建95%置信区间的字符串表示
  EC_2021$Num_2021 <- paste(EC_2021$lower, EC_2021$upper, sep = '-')  # 格式：lower-upper
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021, ')', sep = '')  # 添加右括号
  EC_2021$Num_2021 <- paste('(', EC_2021$Num_2021, sep = '')  # 添加左括号
  EC_2021$Num_2021 <- paste(EC_2021$val, EC_2021$Num_2021, sep = ' ')  # 格式：val (lower-upper)
  
  # 提取1990年年龄标准化率（ASR）
  ASR_1990 <- subset(EC, 
                     EC$year == 1990 & 
                     EC$age_id == 27 &  # 27表示年龄标准化
                     EC$metric_id == 3 &  # 3表示年龄标准化率
                     EC$measure_id == measure)
  
  # 去重，保留每个地区的唯一记录
  ASR_1990 <- ASR_1990 %>% distinct(location_name, .keep_all = TRUE)
  
  # 选择需要的列：地区名称、数值、下限、上限
  ASR_1990 <- ASR_1990[, c(4, 14, 15, 16)]
  
  # 四舍五入数值
  ASR_1990$val <- round(ASR_1990$val, 1)  # 四舍五入到小数点后1位
  ASR_1990$lower <- round(ASR_1990$lower, 1)  # 四舍五入到小数点后1位
  ASR_1990$upper <- round(ASR_1990$upper, 1)  # 四舍五入到小数点后1位
  
  # 构建95%置信区间的字符串表示
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower, ASR_1990$upper, sep = '-')  # 格式：lower-upper
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990, ')', sep = '')  # 添加右括号
  ASR_1990$ASR_1990 <- paste('(', ASR_1990$ASR_1990, sep = '')  # 添加左括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val, ASR_1990$ASR_1990, sep = ' ')  # 格式：val (lower-upper)
  
  # 提取2021年年龄标准化率（ASR）
  ASR_2021 <- subset(EC, 
                     EC$year == 2021 & 
                     EC$age_name == 'Age-standardized' &  # 年龄标准化
                     EC$metric_id == 3 &  # 3表示年龄标准化率
                     EC$measure_id == measure)
  
  # 去重，保留每个地区的唯一记录
  ASR_2021 <- ASR_2021 %>% distinct(location_name, .keep_all = TRUE)
  
  # 选择需要的列：地区名称、数值、下限、上限
  ASR_2021 <- ASR_2021[, c(4, 14, 15, 16)]
  
  # 四舍五入数值
  ASR_2021$val <- round(ASR_2021$val, 1)  # 四舍五入到小数点后1位
  ASR_2021$lower <- round(ASR_2021$lower, 1)  # 四舍五入到小数点后1位
  ASR_2021$upper <- round(ASR_2021$upper, 1)  # 四舍五入到小数点后1位
  
  # 构建95%置信区间的字符串表示
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower, ASR_2021$upper, sep = '-')  # 格式：lower-upper
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021, ')', sep = '')  # 添加右括号
  ASR_2021$ASR_2021 <- paste('(', ASR_2021$ASR_2021, sep = '')  # 添加左括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val, ASR_2021$ASR_2021, sep = ' ')  # 格式：val (lower-upper)
  
  # 提取用于计算EAPC的数据
  EAPC <- subset(EC, 
                 EC$age_name == 'Age-standardized' & 
                 EC$metric_name == 'Rate' &
                 EC$measure_id == measure)
  
  # 选择需要的列：地区名称、年份、数值
  EAPC <- EAPC[, c(4, 13, 14)]
  
  # 获取国家列表
  country <- ASR_1990$location_name
  
  # 创建EAPC计算结果数据框
  EAPC_cal <- data.frame(location_name = country, 
                         EAPC = rep(0, times = 27),  # 初始化EAPC值
                         UCI = rep(0, times = 27),  # 初始化上置信区间
                         LCI = rep(0, times = 27))  # 初始化下置信区间
  
  # 循环计算每个国家的EAPC
  for (i in 1:27) {  # 假设有27个国家
    country_cal <- as.character(EAPC_cal[i, 1])  # 获取当前国家名称
    a <- subset(EAPC, EAPC$location_name == country_cal)  # 提取当前国家的数据
    
    # 计算EAPC：对数值取自然对数
    a$y <- log(a$val)  # 对数转换
    
    # 线性回归模型
    mod_simp_reg <- lm(y ~ year, data = a)  # y = log(val) 对 year 回归
    
    # 计算EAPC值：exp(beta) - 1，乘以100转换为百分比
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100
    
    # 计算95%置信区间下限
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100
    
    # 计算95%置信区间上限
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100
    
    # 保存计算结果
    EAPC_cal[i, 2] <- estimate  # EAPC值
    EAPC_cal[i, 4] <- low  # 下置信区间
    EAPC_cal[i, 3] <- high  # 上置信区间
  }
  
  # 四舍五入EAPC值
  EAPC_cal$EAPC <- round(EAPC_cal$EAPC, 2)  # 四舍五入到小数点后2位
  EAPC_cal$UCI <- round(EAPC_cal$UCI, 2)  # 四舍五入到小数点后2位
  EAPC_cal$LCI <- round(EAPC_cal$LCI, 2)  # 四舍五入到小数点后2位
  
  # 构建EAPC的95%置信区间字符串表示
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI, EAPC_cal$UCI, sep = '-')  # 格式：LCI-UCI
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI, ')', sep = '')  # 添加右括号
  EAPC_cal$EAPC_CI <- paste('(', EAPC_cal$EAPC_CI, sep = '')  # 添加左括号
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC, EAPC_cal$EAPC_CI, sep = ' ')  # 格式：EAPC (LCI-UCI)
  
  # 选择需要的列
  EC_1990 <- EC_1990[, c(1, 6)]  # 地区名称和1990年数值
  ASR_1990 <- ASR_1990[, c(1, 5)]  # 地区名称和1990年ASR
  EC_2021 <- EC_2021[, c(1, 6)]  # 地区名称和2021年数值
  ASR_2021 <- ASR_2021[, c(1, 5)]  # 地区名称和2021年ASR
  EAPC_cal <- EAPC_cal[, c(1, 5)]  # 地区名称和EAPC
  
  # 合并数据
  Results <- merge(EC_1990, ASR_1990, by = 'location_name')  # 合并1990年数据
  Results <- merge(Results, EC_2021, by = 'location_name')  # 合并2021年数据
  Results <- merge(Results, ASR_2021, by = 'location_name')  # 合并2021年ASR
  Results <- merge(Results, EAPC_cal, by = 'location_name')  # 合并EAPC数据
  
  # 返回结果
  return(Results)
}

# 函数调用示例
# 计算胃癌的死亡率EAPC
stomach_Deaths <- EAPC_count(measure = 1)  # measure = 1 表示死亡率

# 计算胃癌的DALYs EAPC
stomach_DALYS <- EAPC_count(measure = 2)  # measure = 2 表示DALYs

# 计算胃癌的发病率EAPC
stomach_Incidence <- EAPC_count(measure = 6)  # measure = 6 表示发病率

# 注意：在调用此函数前，需要确保EC数据框已经加载到环境中
