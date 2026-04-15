################################################################################
# arima.r脚本
# 功能：使用ARIMA模型对全球不同年龄组的DALYs数据进行预测（2022-2050年）
# 输入：measure_2_age_id.csv
# 输出：daly_forecast_2022_2050_Global.csv
################################################################################

# 加载必要的包
library(tidyverse)  # 用于数据处理
library(forecast)   # 用于时间序列分析和预测
library(readr)      # 用于读取CSV文件

# 读取CSV文件数据
cat("正在读取数据...\n")
data <- read_csv("C:/Users/18511/Desktop/结直肠/measure_2_age_id.csv")

# 筛选数据：metric_id = 1（数量），location_name = "Global"，年份1990-2021
cat("正在筛选数据...\n")
filtered_data <- data %>%
  filter(metric_id == 1, location_name == "Global", year >= 1990 & year <= 2021)

# 感兴趣的年龄组列表
age_groups <- c("15-19 years", "20-24 years", "25-29 years", "30-34 years", 
                "35-39 years", "40-44 years", "45-49 years")

# 创建空列表存储所有年龄组的预测结果
forecast_list <- list()

# 创建空数据框存储所有预测结果
forecast_df <- data.frame()

# 遍历每个年龄组
cat("正在处理各年龄组数据...\n")
for (age_group in age_groups) {
  # 筛选当前年龄组的数据
  country_age_data <- filtered_data %>%
    filter(age_name == age_group) %>%  # 筛选当前年龄组
    select(year, val) %>%  # 选择年份和值
    arrange(year)  # 按年份排序
  
  # 检查当前年龄组是否有数据
  if (nrow(country_age_data) > 0) {
    # 创建时间序列对象
    ts_data <- ts(country_age_data$val, start = 1990, frequency = 1)
    
    # 绘制ACF和PACF图，用于确定差分阶数(d)、p和q
    par(mfrow=c(1, 2))
    acf(ts_data, main = paste("ACF - Global", age_group))
    pacf(ts_data, main = paste("PACF - Global", age_group))
    
    # 应用差分使序列平稳（如果需要）
    ndiffs(ts_data)  # 确定是否需要差分
    ts_data_diff <- diff(ts_data, differences = 1)  # 一阶差分
    
    # 检查差分后数据的ACF和PACF
    par(mfrow=c(1, 2))
    acf(ts_data_diff, main = paste("ACF after differencing - Global", age_group))
    pacf(ts_data_diff, main = paste("PACF after differencing - Global", age_group))
    
    # 使用auto.arima()拟合ARIMA模型，自动确定最优的p, d, q
    arima_model <- auto.arima(ts_data)
    
    # 打印ARIMA模型摘要
    print(paste("ARIMA model for Global", age_group, ":"))
    print(summary(arima_model))
    
    # 预测2022年到2050年
    forecast_result <- forecast(arima_model, h = 29)  # 29年预测（2022-2050）
    
    # 将预测结果存储到列表中
    forecast_list[[paste("Global", age_group, sep = "_")]] <- forecast_result
    
    # 准备CSV输出数据
    forecast_years <- 2022:2050
    forecast_values <- forecast_result$mean
    forecast_data <- data.frame(
      location_name = rep("Global", length(forecast_years)),
      age_group = rep(age_group, length(forecast_years)),
      year = forecast_years,
      forecast = forecast_values
    )
    
    # 将预测数据添加到总体预测数据框
    forecast_df <- bind_rows(forecast_df, forecast_data)
    
    # 残差诊断
    residuals <- residuals(arima_model)
    
    # 绘制残差图
    par(mfrow=c(1, 2))
    plot(residuals, main = paste("Residuals for Global", age_group), ylab = "Residuals")
    acf(residuals, main = paste("ACF of Residuals - Global", age_group))
    
    # Ljung-Box检验残差自相关性
    lb_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
    print(paste("Ljung-Box test for residuals of Global", age_group, ":"))
    print(lb_test)
    
    # 残差的QQ图
    qqnorm(residuals)
    qqline(residuals, col = "red")
    
    # 计算预测精度指标（ME, RMSE, MAE, MPE, MAPE, MASE）
    accuracy_metrics <- accuracy(forecast_result)
    print(paste("Forecast accuracy metrics for Global", age_group, ":"))
    print(accuracy_metrics)
  }
}

# 将预测结果写入CSV文件
cat("正在保存预测结果...\n")
write_csv(forecast_df, "C:/Users/18511/Desktop/结直肠daly_forecast_2022_2050_Global.csv")

# 为特定年龄组生成预测图
specific_age_group <- "15-19 years"  # 修改此行以选择年龄组

# 生成特定年龄组的预测图
forecast_key <- paste("Global", specific_age_group, sep = "_")

if (forecast_key %in% names(forecast_list)) {
  forecast_result <- forecast_list[[forecast_key]]
  # 绘制特定年龄组的预测图
  plot(forecast_result, main = paste("DALYs Forecast for Global Age Group:", specific_age_group),
       xlab = "Year", ylab = "DALYs")
} else {
  print("Forecast for the specified age group is not available.")
}

cat("预测完成！\n")
