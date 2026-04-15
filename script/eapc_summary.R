################################################################################
# EAPC汇总表脚本
# 功能：生成全球消化系统癌症的汇总表，包含各地区和各疾病的EAPC数据
# 输入：All_Disease_Results.Rdata, original_data.Rdata, order.Rdata
# 输出：Global_Digestive_Cancers_Summary_country.xlsx
################################################################################

# 加载必要的包
library(tidyverse)  # 用于数据处理
library(openxlsx)   # 用于Excel文件操作

# 加载数据
cat("正在加载数据...\n")
load("data/processed/All_Disease_Results.Rdata")
load("data/processed/original_data.Rdata")
load("data/processed/order.Rdata")
#order <- order_country  # 注释掉，使用默认的order数据

################################################################################
# extract_global_data函数
# 功能：提取全球数据
# 参数：
#   - disease_list: 包含疾病数据的列表
# 返回值：全球数据的数据框
################################################################################
extract_global_data <- function(disease_list) {
  map_dfr(names(disease_list), function(disease){
    # 提取三种指标
    deaths <- disease_list[[disease]]$Death$table %>% 
      filter(location_name == "Global") %>%  # 筛选全球数据
      select(-location_name) %>%  # 移除地区列
      rename_with(~paste0("Deaths_", .x))  # 重命名列
    
    dalys <- disease_list[[disease]]$DALYS$table %>% 
      filter(location_name == "Global") %>%  # 筛选全球数据
      select(-location_name) %>%  # 移除地区列
      rename_with(~paste0("DALYs_", .x))  # 重命名列
    
    incidence <- disease_list[[disease]]$Incidence$table %>% 
      filter(location_name == "Global") %>%  # 筛选全球数据
      select(-location_name) %>%  # 移除地区列
      rename_with(~paste0("Incidence_", .x))  # 重命名列
    
    # 合并列
    bind_cols(
      Region = "Global",  # 添加地区列
      Disease = disease,  # 添加疾病列
      deaths,
      dalys,
      incidence
    )
  })
}

# 生成各疾病Global数据 -----------------------------------------------------------

cat("正在生成各疾病Global数据...\n")

# 各疾病单独数据
disease_data <- list(
  stomach = stomach,      # 胃癌
  liver = liver,          # 肝癌
  cavity = cavity,        # 口腔癌
  colon = colon,          # 结直肠癌
  pancreatic = pancreatic  # 胰腺癌
)

global_by_disease <- extract_global_data(disease_data)

# 查看数据结构
cat("查看数据结构...\n")
glimpse(global_by_disease)

# 合并所有疾病的原始数据
cat("正在合并所有疾病的原始数据...\n")
all_diseases_combined <- bind_rows(
  stomach_data, liver_data, cavity_data, 
  colon_data, pancreatic_data,
  .id = "disease"  # 添加疾病标识
) %>%
  mutate(disease = recode(disease,
                          "1" = "stomach",
                          "2" = "liver",
                          "3" = "colon",
                          "4" = "pancreatic",
                          "5" = "cavity"
  ))

# 计算合计数据
cat("正在计算合计数据...\n")
global_total <- all_diseases_combined %>%
  filter(location_name == "Global") %>%  # 筛选全球数据
  group_by(measure_id, year, age_id, metric_id) %>%  # 分组
  summarise(
    val = sum(val, na.rm = TRUE),  # 求和
    lower = sum(lower, na.rm = TRUE),
    upper = sum(upper, na.rm = TRUE),
    .groups = "drop"  # 取消分组
  )

################################################################################
# EAPC_count_combined函数
# 功能：计算EAPC数据
# 参数：
#   - data: 输入数据
#   - measure: 测量指标
# 返回值：包含1990年数据、2021年数据和EAPC的结果表
################################################################################
EAPC_count_combined <- function(data, measure) {
  # 确保包含必要列
  required_cols <- c("location_name", "year", "val", "lower", "upper")
  stopifnot(all(required_cols %in% colnames(data)))  # 检查必需列
  
  # 计算1990年数据
  EC_1990 <- data %>%
    filter(year == 1990) %>%  # 筛选1990年数据
    select(location_name, val, lower, upper) %>%  # 选择需要的列
    distinct(location_name, .keep_all = TRUE) %>%  # 去重
    mutate(
      across(c(val, lower, upper), ~round(.x, 1)),  # 四舍五入到1位小数
      Num_1990 = sprintf("%.1f (%.1f-%.1f)", val, lower, upper)  # 格式化输出
    ) %>%
    select(location_name, Num_1990)  # 选择需要的列
  
  # 计算2021年数据
  EC_2021 <- data %>%
    filter(year == 2021) %>%  # 筛选2021年数据
    select(location_name, val, lower, upper) %>%  # 选择需要的列
    distinct(location_name, .keep_all = TRUE) %>%  # 去重
    mutate(
      across(c(val, lower, upper), ~round(.x, 1)),  # 四舍五入到1位小数
      Num_2021 = sprintf("%.1f (%.1f-%.1f)", val, lower, upper)  # 格式化输出
    ) %>%
    select(location_name, Num_2021)  # 选择需要的列
  
  # 计算EAPC
  EAPC_data <- data %>%
    group_by(location_name) %>%  # 按地区分组
    do({
      mod <- lm(log(val) ~ year, data = .)  # 拟合线性模型
      ci <- confint(mod)[2,]  # 计算置信区间
      data.frame(
        EAPC = (exp(coef(mod)[2])-1)*100,  # 计算EAPC
        LCI = (exp(ci[1])-1)*100,  # 计算下限
        UCI = (exp(ci[2])-1)*100  # 计算上限
      )
    }) %>%
    ungroup() %>%  # 取消分组
    mutate(
      across(c(EAPC, LCI, UCI), ~round(.x, 2)),  # 四舍五入到2位小数
      EAPC_CI = sprintf("%.2f (%.2f-%.2f)", EAPC, LCI, UCI)  # 格式化输出
    ) %>%
    select(location_name, EAPC_CI)  # 选择需要的列
  
  # 合并结果
  Results_table <- EC_1990 %>%
    full_join(EC_2021, by = "location_name") %>%  # 合并1990年和2021年数据
    full_join(EAPC_data, by = "location_name")  # 合并EAPC数据
  
  return(Results_table)
}

################################################################################
# calculate_combined_by_region函数
# 功能：按地区计算所有疾病的合并数据
# 返回值：各地区的合并数据
################################################################################
calculate_combined_by_region <- function() {
  # 获取地区顺序（从order表中）
  regions <- order$location_name
  
  # 初始化结果数据框
  all_results <- data.frame()
  
  # 遍历每个地区
  for (region in regions) {
    cat(paste("正在处理地区:", region, "...\n"))
    
    # 汇总当前地区的数据
    region_total <- all_diseases_combined %>%
      filter(location_name == region) %>%  # 筛选当前地区
      group_by(location_name, measure_id, year, age_id, metric_id) %>%  # 分组
      summarise(
        val = sum(val, na.rm = TRUE),  # 求和
        lower = sum(lower, na.rm = TRUE),
        upper = sum(upper, na.rm = TRUE),
        .groups = "drop"  # 取消分组
      )
    
    # 计算各指标（原始数值）
    deaths_raw <- EAPC_count_combined(
      region_total %>% filter(measure_id == 1 & age_id == 24 & metric_id == 1), 1
    ) %>%
      rename_with(~paste0("Deaths_", .x), -location_name)
    
    dalys_raw <- EAPC_count_combined(
      region_total %>% filter(measure_id == 2 & age_id == 24 & metric_id == 1), 2
    ) %>%
      rename_with(~paste0("DALYs_", .x), -location_name)
    
    incidence_raw <- EAPC_count_combined(
      region_total %>% filter(measure_id == 6 & age_id == 24 & metric_id == 1), 6
    ) %>%
      rename_with(~paste0("Incidence_", .x), -location_name)
    
    # 计算ASR指标
    deaths_asr <- EAPC_count_combined(
      region_total %>% filter(measure_id == 1 & age_id == 27 & metric_id == 3), 1
    ) %>%
      rename_with(~paste0("Deaths_", .x), -location_name)
    
    dalys_asr <- EAPC_count_combined(
      region_total %>% filter(measure_id == 2 & age_id == 27 & metric_id == 3), 2
    ) %>%
      rename_with(~paste0("DALYs_", .x), -location_name)
    
    incidence_asr <- EAPC_count_combined(
      region_total %>% filter(measure_id == 6 & age_id == 27 & metric_id == 3), 6
    ) %>%
      rename_with(~paste0("Incidence_", .x), -location_name)
    
    # 合并当前地区的结果
    region_results <- bind_cols(
      Region = region,
      Disease = "All Five Cancers Combined",
      deaths_raw %>% select(-location_name),
      deaths_asr %>% select(-location_name),
      dalys_raw %>% select(-location_name),
      dalys_asr %>% select(-location_name),
      incidence_raw %>% select(-location_name),
      incidence_asr %>% select(-location_name)
    )
    
    # 重命名列以匹配所需格式
    colnames(region_results) <- c(
      "Region", "Disease",
      "Deaths_Num_1990", "Deaths_Num_2021", "Deaths_EAPC_CI",
      "Deaths_ASR_1990", "Deaths_ASR_2021", "Deaths_ASR_EAPC_CI",
      "DALYs_Num_1990", "DALYs_Num_2021", "DALYs_EAPC_CI",
      "DALYs_ASR_1990", "DALYs_ASR_2021", "DALYs_ASR_EAPC_CI",
      "Incidence_Num_1990", "Incidence_Num_2021", "Incidence_EAPC_CI",
      "Incidence_ASR_1990", "Incidence_ASR_2021", "Incidence_ASR_EAPC_CI"
    )
    
    # 选择并重新排列列
    final_order <- c(
      "Region", "Disease",
      "Deaths_Num_1990", "Deaths_ASR_1990", "Deaths_Num_2021", "Deaths_ASR_2021", "Deaths_EAPC_CI",
      "DALYs_Num_1990", "DALYs_ASR_1990", "DALYs_Num_2021", "DALYs_ASR_2021", "DALYs_EAPC_CI",
      "Incidence_Num_1990", "Incidence_ASR_1990", "Incidence_Num_2021", "Incidence_ASR_2021", "Incidence_EAPC_CI"
    )
    
    region_results <- region_results %>% 
      select(all_of(final_order)) %>%
      rename(
        Deaths_EAPC_CI = Deaths_EAPC_CI,
        DALYs_EAPC_CI = DALYs_EAPC_CI,
        Incidence_EAPC_CI = Incidence_EAPC_CI
      )
    
    # 添加到总结果中
    all_results <- bind_rows(all_results, region_results)
  }
  
  return(all_results)
}

# 计算所有地区的合并数据
cat("正在计算所有地区的合并数据...\n")
all_regions_combined <- calculate_combined_by_region()

# 查看结果
cat("查看结果...\n")
head(all_regions_combined)

# 合并合计数据与各疾病数据
cat("正在合并合计数据与各疾病数据...\n")
final_table <- bind_rows(
  all_regions_combined,
  global_by_disease
) %>%
  select(Region, Disease, 
         starts_with("Deaths"), 
         starts_with("DALYs"), 
         starts_with("Incidence"))

# 导出Excel
cat("正在导出Excel文件...\n")
wb <- createWorkbook()  # 创建工作簿
addWorksheet(wb, "Global_Summary")  # 添加工作表
writeData(wb, "Global_Summary", final_table)  # 写入数据

# 添加格式
header_style <- createStyle(
  textDecoration = "BOLD",  # 粗体
  fgFill = "#4F81BD",  # 背景色
  fontColour = "white",  # 字体颜色
  halign = "center"  # 居中对齐
)

addStyle(wb, sheet = 1, header_style, rows = 1, cols = 1:ncol(final_table))  # 添加表头样式
setColWidths(wb, sheet = 1, cols = 1:ncol(final_table), widths = "auto")  # 自动调整列宽

saveWorkbook(wb, "Global_Digestive_Cancers_Summary_country.xlsx", overwrite = TRUE)  # 保存工作簿

cat("数据处理完成！\n")
