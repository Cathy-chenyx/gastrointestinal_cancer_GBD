################################################################################
# MAP_all脚本
# 功能：生成消化系统癌症的全球地图，包括ASR、病例变化和EAPC地图
# 输入：Combined_Country_Data.Rdata
# 输出：消化系统癌症的全球地图图片
################################################################################

# 安装必要的包（如果未安装）
# install.packages('ggmap')
# install.packages('rgdal')
# install.packages('maps')
# install.packages('dplyr')

# 加载必要的包
library(ggplot2)    # 用于数据可视化
library(ggmap)      # 用于地图可视化
library(maps)       # 用于地图数据
library(dplyr)      # 用于数据处理
library(sf)         # 用于空间数据处理
library(patchwork)  # 用于图形组合

# 加载合并后的数据
cat("正在加载数据...\n")
load("data/processed/Combined_Country_Data.Rdata")

################################################################################
# generate_combined_map函数
# 功能：生成合并癌症的全球地图
# 参数：
#   - data: 输入数据
#   - name: 输出文件的名称前缀
# 返回值：无，直接保存地图图片
################################################################################
generate_combined_map <- function(data, name) {
  EC <- data
  
  # 筛选非空地区
  EC <- EC %>%
    filter(!is.na(location_name) & location_name != "")
  
  # ASR地图（2021年）
  cat("正在处理ASR数据...\n")
  ASR_2021 <- EC %>%
    filter(year == 2021 & 
             age_id == 27 & 
             metric_id == 3 &
             measure_id == 6) %>%  # 筛选2021年ASR数据
    distinct(location_name, .keep_all = TRUE) %>%  # 去重
    select(location_name, val, lower, upper) %>%  # 选择需要的列
    mutate(
      val = round(val, 1),  # 四舍五入到1位小数
      lower = round(lower, 1),
      upper = round(upper, 1)
    )
  
  # 重命名列
  names(ASR_2021)[1] <- "location"
  
  # 病例变化地图（1990-2021）
  cat("正在处理病例变化数据...\n")
  case_1990 <- EC %>%
    filter(year == 1990 & 
             age_id == 24 & 
             metric_id == 1 &
             measure_id == 6) %>%  # 筛选1990年病例数据
    select(location_name, val) %>%  # 选择需要的列
    rename(case_1990 = val)  # 重命名列
  
  case_2021 <- EC %>%
    filter(year == 2021 & 
             age_id == 24 & 
             metric_id == 1 &
             measure_id == 6) %>%  # 筛选2021年病例数据
    select(location_name, val) %>%  # 选择需要的列
    rename(case_2021 = val)  # 重命名列
  
  # 计算病例变化百分比
  country_asr <- full_join(case_1990, case_2021, by = "location_name") %>%
    mutate(val = (case_2021 - case_1990) / case_1990 * 100)  # 计算变化百分比
  
  # 重命名列
  names(country_asr)[1] <- "location"
  
  # EAPC地图
  cat("正在处理EAPC数据...\n")
  EAPC_data <- EC %>%
    filter(age_id == 27 & 
             metric_id == 3 &
             measure_id == 6) %>%  # 筛选EAPC计算所需数据
    select(location_name, year, val)  # 选择需要的列
  
  # 计算EAPC
  EAPC_cal <- data.frame(location = unique(EAPC_data$location_name))  # 初始化EAPC计算结果
  
  for (i in 1:nrow(EAPC_cal)) {
    country_cal <- EAPC_cal$location[i]  # 获取当前国家
    a <- EAPC_data %>% 
      filter(location_name == country_cal) %>%  # 筛选当前国家的数据
      mutate(y = log(val))  # 计算对数值
    
    if (nrow(a) > 1) {  # 确保有足够的数据点
      mod_simp_reg <- lm(y ~ year, data = a)  # 拟合线性模型
      estimate <- (exp(coef(mod_simp_reg)[2]) - 1) * 100  # 计算EAPC
      low <- (exp(confint(mod_simp_reg)[2, 1]) - 1) * 100  # 计算下限
      high <- (exp(confint(mod_simp_reg)[2, 2]) - 1) * 100  # 计算上限
      EAPC_cal$EAPC[i] <- estimate
      EAPC_cal$LCI[i] <- low
      EAPC_cal$UCI[i] <- high
    } else {
      EAPC_cal$EAPC[i] <- NA  # 数据不足时设为NA
      EAPC_cal$LCI[i] <- NA
      EAPC_cal$UCI[i] <- NA
    }
  }
  
  ################################################################################
  # standardize_country_names函数
  # 功能：标准化国家名称，确保与地图数据匹配
  # 参数：
  #   - df: 包含国家名称的数据框
  # 返回值：标准化后的国家名称数据框
  ################################################################################
  standardize_country_names <- function(df) {
    # 标准化国家名称
    df$location[df$location == 'United States of America'] = 'USA'
    df$location[df$location == 'Russian Federation'] = 'Russia'
    df$location[df$location == 'United Kingdom'] = 'UK'
    df$location[df$location == 'Congo'] = 'Republic of Congo'
    df$location[df$location == "Iran (Islamic Republic of)"] = 'Iran'
    df$location[df$location == "Democratic People's Republic of Korea"] = 'North Korea'
    df$location[df$location == "Taiwan (Province of China)"] = 'Taiwan'
    df$location[df$location == "Republic of Korea"] = 'South Korea'
    df$location[df$location == "United Republic of Tanzania"] = 'Tanzania'
    df$location[df$location == "C?te d'Ivoire"] = 'Saint Helena'
    df$location[df$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
    df$location[df$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
    df$location[df$location == "Czechia"] = 'Czech Republic'
    df$location[df$location == "Republic of Moldova"] = 'Moldova'
    df$location[df$location == "Viet Nam"] = 'Vietnam'
    df$location[df$location == "Lao People's Democratic Republic"] = 'Laos'
    df$location[df$location == "Syrian Arab Republic"] = 'Syria'
    df$location[df$location == "North Macedonia"] = 'Macedonia'
    df$location[df$location == "Micronesia (Federated States of)"] = 'Micronesia'
    df$location[df$location == "Macedonia"] = 'North Macedonia'
    df$location[df$location == "Trinidad and Tobago"] = 'Trinidad'
    df <- rbind(df, df[df$location == "Trinidad",])  # 复制Trinidad数据
    df$location[df$location == "Trinidad"] = 'Tobago'  # 重命名为Tobago
    df$location[df$location == "Cabo Verde"] = 'Cape Verde'
    df$location[df$location == "United States Virgin Islands"] = 'Virgin Islands'
    df$location[df$location == "Antigua and Barbuda"] = 'Antigu'
    df <- rbind(df, df[df$location == "Antigu",])  # 复制Antigu数据
    df$location[df$location == "Antigu"] = 'Barbuda'  # 重命名为Barbuda
    df$location[df$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
    df <- rbind(df, df[df$location == "Saint Kitts",])  # 复制Saint Kitts数据
    df$location[df$location == "Saint Kitts"] = 'Nevis'  # 重命名为Nevis
    df$location[df$location == "Côte d'Ivoire"] = 'Ivory Coast'
    df$location[df$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
    df <- rbind(df, df[df$location == "Saint Vincent",])  # 复制Saint Vincent数据
    df$location[df$location == "Saint Vincent"] = 'Grenadines'  # 重命名为Grenadines
    df$location[df$location == "Eswatini"] = 'Swaziland'
    df$location[df$location == "Brunei Darussalam"] = 'Brunei'
    return(df)
  }

  # 标准化名称
  cat("正在标准化国家名称...\n")
  ASR_2021 <- standardize_country_names(ASR_2021)
  country_asr <- standardize_country_names(country_asr)
  EAPC_cal <- standardize_country_names(EAPC_cal)
  
  # 绘制ASR地图
  cat("正在绘制ASR地图...\n")
  worldData <- map_data('world')  # 获取世界地图数据
  total_asr <- full_join(worldData, ASR_2021, by = c('region' = 'location'))  # 合并数据
  
  # 对ASR值进行分箱
  total_asr <- total_asr %>% 
    mutate(val2 = cut(val, breaks = c(0, 10, 15, 30, 50, Inf),
                      labels = c("0~10", "10~15", "15~30", "30~50", "50+")))
  
  # 绘制ASR地图
  p_asr <- ggplot() +
    geom_polygon(data = total_asr, 
                 aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +  # 绘制多边形
    scale_fill_brewer(palette = "Reds", na.value = "grey") +  # 使用红色调色板
    theme_void() +  # 使用空白主题
    labs(title = "Age-Standardized Incidence Rate (2021)", 
         subtitle = "Per 100,000 population",
         fill = 'ASR') +  # 设置标题和图例
    theme(legend.position = 'right',
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))  # 设置主题
  
  # 绘制病例变化地图
  cat("正在绘制病例变化地图...\n")
  total_case <- full_join(worldData, country_asr, by = c('region' = 'location'))  # 合并数据
  
  # 对病例变化值进行分箱
  total_case <- total_case %>% 
    mutate(val2 = cut(val, breaks = c(-60, -30, 0, 50, 100, 200, 300, 1200),
                      labels = c("30% to 60% decrease", "<30% decrease", "<50% increase",
                                 "50% to 100% increase", "100% to 200% increase", 
                                 "200% to 300% increase", ">300% increase")))
           
  # 绘制病例变化地图
  p_case <- ggplot() +
    geom_polygon(data = total_case, 
                 aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +  # 绘制多边形
    scale_fill_manual(values = c("#006400", "#66CD00", "#FFE4C4", "#FF7256", "#FF4040", "#CD3333", "#8B2323"),
                      na.value = "grey") +  # 使用自定义颜色
    theme_void() +  # 使用空白主题
    labs(title = "Change in Cancer Cases (1990-2021)", fill = 'Change') +  # 设置标题和图例
    theme(legend.position = 'right',
          plot.title = element_text(hjust = 0.5, face = "bold"))  # 设置主题
           
  # 绘制EAPC地图
  cat("正在绘制EAPC地图...\n")
  total_eapc <- full_join(worldData, EAPC_cal, by = c('region' = 'location'))  # 合并数据
  
  # 绘制EAPC地图
  p_eapc <- ggplot() +
    geom_polygon(data = total_eapc, 
                 aes(x = long, y = lat, group = group, fill = EAPC),
                 colour = "black", size = .2) +  # 绘制多边形
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, na.value = "grey") +  # 使用渐变颜色
    theme_void() +  # 使用空白主题
    labs(title = "Estimated Annual Percentage Change (EAPC)", 
         subtitle = "1990-2021 Trend",
         fill = 'EAPC (%)') +  # 设置标题和图例
    theme(legend.position = 'right',
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))  # 设置主题
    
  # 使用 patchwork 将三张图横向排列，并为每张图添加 A/B/C 标签
  cat("正在组合地图...\n")
  combined_plot <- (p_asr | p_case | p_eapc) +
    plot_layout(ncol = 3, widths = c(1, 1, 1)) +  # 设置列为3，宽度相等
    plot_annotation(tag_levels = 'A')  # 自动添加 A, B, C 标签
  
  # 保存图形
  cat("正在保存地图...\n")
  dir.create("outcome/MAP", recursive = TRUE, showWarnings = FALSE)
  ggsave(filename = paste0("outcome/MAP/", name, "_Combined_ASR.png"), p_asr, width = 12, height = 8, dpi = 300)
  ggsave(filename = paste0("outcome/MAP/", name, "_Combined_CaseChange.png"), p_case, width = 12, height = 8, dpi = 300)
  ggsave(filename = paste0("outcome/MAP/", name, "_Combined_EAPC.png"), p_eapc, width = 12, height = 8, dpi = 300)
  ggsave(filename = paste0("outcome/MAP/", name, "_Combined.png"), combined_plot, width = 24, height = 6, dpi = 300)
  
  cat("地图保存完成！\n")
}

# 生成合并癌症地图
cat("正在生成消化系统癌症全球地图...\n")
generate_combined_map(combined_country_data, "Digestive_Cancers")

