################################################################################
# 地理可视化脚本
# 功能：生成全球癌症数据的地理分布图，包括发病率（ASR）、病例变化和EAPC的地图
# 输入：合并后的疾病数据
# 输出：地理分布图，保存在outcome/MAP目录中
################################################################################

# 设置工作路径
setwd('..')  # 设置工作路径为项目根目录

# 安装必要的包（如果尚未安装）
# install.packages('ggmap')
# install.packages('rgdal')
# install.packages('maps')
# install.packages('dplyr')
# install.packages("sf")  # 安装替代包

# 加载必要的包
library(ggplot2)  # 用于数据可视化
library(ggmap)     # 用于地图数据
library(maps)      # 用于地图数据
library(dplyr)     # 用于数据处理
library(sf)        # 替代 rgdal 的功能

# 加载合并后的数据
cat("正在加载数据...\n")
load("data/processed/original_data.Rdata")  # 加载合并后的疾病数据

# 生成地图函数 ----------------------------------------------------------------

################################################################################
# generate_map函数
# 功能：生成三种类型的地图（ASR、病例变化、EAPC）
# 参数：
#   - data: 输入数据
#   - name: 输出文件名前缀
# 返回值：无，直接保存地图文件
################################################################################
generate_map <- function(data, name){
  # 重命名数据框
  EC <- data
  
  # 过滤掉位置名称为空或NA的记录
  EC <- EC %>%
    filter(!is.na(location_name) & location_name != "")
  
  # 提取2021年年龄标准化发病率数据
  ASR_2021 <- subset(EC, 
                     EC$year == 2021 & 
                     EC$age_name == 'Age-standardized' & 
                     EC$metric_id == 3 &  # 3表示年龄标准化率
                     EC$measure_id == 6)  # 6表示发病率
  
  # 去重，保留每个地区的唯一记录
  ASR_2021 <- ASR_2021 %>% distinct(location_name, .keep_all = TRUE)
  
  # 选择需要的列：地区名称、数值、下限、上限
  ASR_2021 <- ASR_2021[, c(4, 14, 15, 16)]
  
  # 四舍五入数值
  ASR_2021$val <- round(ASR_2021$val, 1)  # 四舍五入到小数点后1位
  ASR_2021$lower <- round(ASR_2021$lower, 1)  # 四舍五入到小数点后1位
  ASR_2021$upper <- round(ASR_2021$upper, 1)  # 四舍五入到小数点后1位
  
  # 重命名地区名称列为"location"
  names(ASR_2021)[1] <- "location"
  
  #### 生成ASR地图
  # 获取世界地图数据
  worldData <- map_data('world')
  
  # 复制数据并转换为字符型
  country_asr <- ASR_2021
  country_asr$location <- as.character(country_asr$location)
  
  # 统一国家名称，使其与worldData中的名称一致
  cat("正在统一国家名称...\n")
  country_asr$location[country_asr$location == 'United States of America'] = 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
  country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
  country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
  
  # 处理特殊情况：特立尼达和多巴哥
  country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
  country_asr <- rbind(country_asr, country_asr[country_asr$location == "Trinidad",])
  country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
  
  # 处理特殊情况：佛得角
  country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
  
  # 处理特殊情况：美属维尔京群岛
  country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
  
  # 处理特殊情况：安提瓜和巴布达
  country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
  country_asr <- rbind(country_asr, country_asr[country_asr$location == "Antigu",])
  country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
  
  # 处理特殊情况：圣基茨和尼维斯
  country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
  country_asr <- rbind(country_asr, country_asr[country_asr$location == "Saint Kitts",])
  country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
  
  # 处理特殊情况：科特迪瓦
  country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
  
  # 处理特殊情况：圣文森特和格林纳丁斯
  country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
  country_asr <- rbind(country_asr, country_asr[country_asr$location == "Saint Vincent",])
  country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
  
  # 处理特殊情况：斯威士兰
  country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
  
  # 处理特殊情况：文莱
  country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
  
  # 合并世界地图数据和ASR数据
  worldData <- map_data('world')
  total <- full_join(worldData, country_asr, by = c('region' = 'location'))
  
  # 创建基础ggplot对象
  p <- ggplot()
  
  # 对ASR值进行分组，用于地图着色
  total <- total %>% mutate(val2 = cut(val, breaks = c(0, 2, 5, 10, 20, 100),
                                       labels = c("0~2.0", "2.0~5.0", "5.0~10.0",
                                                  "10.0~20.0", "20.0+"),  
                                       include.lowest = T, right = T))
  
  # 生成ASR地图
  cat("正在生成ASR地图...\n")
  p_asr <- p + geom_polygon(data = total, 
                            aes(x = long, y = lat, group = group, fill = val2),
                            colour = "black", size = .2) + 
    scale_fill_brewer(palette = "Reds") +  # 使用红色渐变调色板
    theme_void() +  # 使用无坐标轴主题
    labs(x = "", y = "") +  # 无坐标轴标签
    guides(fill = guide_legend(title = 'ASR(/10^5)')) +  # 图例标题
    theme(legend.position = 'right')  # 图例位置
  
  # 疾病数量变化图 ---------------------------------------------------------------------
  
  # 提取2021年病例数
  case_2021 <- subset(EC, 
                     EC$year == 2021 & 
                     EC$age_id == 24 &  # 24表示全年龄段
                     EC$metric_name == 'Number' &  # Number表示绝对数
                     EC$measure_name == 'Incidence')  # Incidence表示发病率
  
  # 提取1990年病例数
  case_1990 <- subset(EC, 
                     EC$year == 1990 & 
                     EC$age_id == 24 &  # 24表示全年龄段
                     EC$metric_name == 'Number' &  # Number表示绝对数
                     EC$measure_name == 'Incidence')  # Incidence表示发病率
  
  # 选择需要的列
  case_1990 <- case_1990[, c(4, 14)]  # 地区名称和数值
  case_2021 <- case_2021[, c(4, 14)]  # 地区名称和数值
  
  # 重命名列
  names(case_1990) <- c('location', 'case_1990')
  names(case_2021) <- c('location', 'case_2021')
  
  # 合并1990年和2021年的病例数数据
  country_asr <- merge(case_1990, case_2021, by = 'location')
  
  # 计算病例变化百分比
  country_asr$val <- (country_asr$case_2021 - country_asr$case_1990) / country_asr$case_1990 * 100
  
  # 统一国家名称
  country_asr$location <- as.character(country_asr$location)
  country_asr$location[country_asr$location == 'United States of America'] = 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
  country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
  
  # 合并世界地图数据和病例变化数据
  worldData <- map_data('world')
  total <- full_join(worldData, country_asr, by = c('region' = 'location'))
  
  # 对变化百分比进行分组，用于地图着色
  total <- total %>% mutate(val2 = cut(val, breaks = c(-60, -30, 0, 50, 100, 200, 300, 1200),
                                       labels = c("30% to 60% decrease", "<30% decrease", "<50% increase",
                                                  "50% to 100% increase", "100% to 200% increase", 
                                                  "200% to 300% increase", ">300% increase"),
                                       include.lowest = T, right = T))
  
  # 生成病例变化地图
  cat("正在生成病例变化地图...\n")
  p_case <- p + geom_polygon(data = total, 
                             aes(x = long, y = lat, group = group, fill = val2),
                             colour = "black", size = .2) + 
    scale_fill_manual(values = c("#006400", "#66CD00", "#FFE4C4", "#FF7256", "#FF4040", "#CD3333", "#8B2323")) +
    theme_void() +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = 'Change in cancer cases')) +
    theme(legend.position = 'right')
  
  # EAPC地图 ------------------------------------------------------------------
  
  # 提取用于计算EAPC的数据
  EAPC <- subset(EC, 
                 EC$age_name == 'Age-standardized' & 
                 EC$metric_name == 'Rate' &
                 EC$measure_name == 'Incidence')
  
  # 选择需要的列：地区名称、年份、数值
  EAPC <- EAPC[, c(4, 13, 14)]
  
  # 获取国家列表
  country <- case_2021$location
  
  # 创建EAPC计算结果数据框
  EAPC_cal <- data.frame(location = country, 
                         EAPC = rep(0, times = length(country)),  # 初始化EAPC值
                         UCI = rep(0, times = length(country)),  # 初始化上置信区间
                         LCI = rep(0, times = length(country)))  # 初始化下置信区间
  
  # 循环计算每个国家的EAPC
  cat("正在计算EAPC...\n")
  for (i in 1:length(country)) {
    country_cal <- as.character(EAPC_cal[i, 1])  # 获取当前国家名称
    a <- subset(EAPC, EAPC$location == country_cal)  # 提取当前国家的数据
    
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
  
  # 复制EAPC计算结果
  country_asr <- EAPC_cal
  
  # 统一国家名称
  country_asr$location <- as.character(country_asr$location)
  country_asr$location[country_asr$location == 'United States of America'] = 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
  country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
  
  # 合并世界地图数据和EAPC数据
  worldData <- map_data('world')
  total <- full_join(worldData, country_asr, by = c('region' = 'location'))
  
  # 生成EAPC地图
  cat("正在生成EAPC地图...\n")
  p_eapc <- p + geom_polygon(data = total, 
                            aes(x = long, y = lat, group = group, fill = EAPC),
                            colour = "black", size = .2) + 
    scale_fill_gradient2(low = "pink", mid = 'white', high = "purple",
                         midpoint = 0) +  # 使用双色渐变，0为中点
    theme_void() +
    labs(x = "", y = "") +
    guides(fill = guide_colorbar(title = 'EAPC')) +
    theme(legend.position = 'right')
  
  # 确保输出目录存在
  dir.create("outcome/MAP", recursive = TRUE, showWarnings = FALSE)
  
  # 保存地图
  cat("正在保存地图...\n")
  ggsave(filename = paste0("outcome/MAP/", name, "_ASR.png"), p_asr, width = 12, height = 8)
  ggsave(filename = paste0("outcome/MAP/", name, "_CaseChange.png"), p_case, width = 12, height = 8)
  ggsave(filename = paste0("outcome/MAP/", name, "_EAPC.png"), p_eapc, width = 12, height = 8)
  
  cat(paste("地图已保存到 outcome/MAP/ 目录，前缀为", name, "\n"))
}

# 生成各类疾病的地图
cat("开始生成各类疾病的地图...\n")
generate_map(colon_data, "colon")  # 结直肠癌
generate_map(stomach_data, "stomach")  # 胃癌
generate_map(pancreatic_data, "pancreatic")  # 胰腺癌
generate_map(cavity_data, "cavity")  # 口腔癌
generate_map(liver_data, "liver")  # 肝癌

cat("所有地图生成完成！\n")




