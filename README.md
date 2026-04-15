# 统计建模大赛项目

## 1. 项目概述

本项目是一个统计建模大赛参赛作品，主要分析全球消化系统癌症（包括胃癌、肝癌、结直肠癌、胰腺癌和口腔癌）的发病率、死亡率和伤残调整生命年（DALYs）数据。项目通过数据预处理、统计分析和地理可视化，探讨癌症负担与社会人口指数（SDI）的关系，并评估癌症趋势的年度百分比变化（EAPC）。

### 1.1 项目目标
- 分析全球消化系统癌症的流行趋势
- 探讨SDI与癌症发病率的相关性
- 计算并可视化癌症趋势的年度百分比变化
- 生成地理分布图，展示全球癌症负担的空间分布

## 2. 项目结构

```
├── data/             # 数据目录
│   ├── raw/          # 原始数据文件
│   │   ├── 口腔/       # 口腔癌数据
│   │   ├── 结直肠/      # 结直肠癌数据
│   │   ├── 肝/        # 肝癌数据
│   │   ├── 胃/        # 胃癌数据
│   │   └── 胰腺/       # 胰腺癌数据
│   └── processed/     # 处理后的数据
├── script/           # R脚本文件
│   ├── data_preprocessing.r              # 原始数据处理
│   ├── merge_datasets.R                  # 数据整合
│   ├── sdi_incidence_correlation.R       # SDI与癌症发病率关系分析
│   ├── eapc_functions.R                  # 年度百分比变化计算
│   ├── map_visualization.R               # 地理可视化
│   ├── eapc_map_all.R                    # 全疾病EAPC地图
│   ├── eapc_table.R                      # EAPC三线表
│   ├── eapc_summary.R                    # EAPC汇总表
│   ├── asr_eapc_bar_chart.R              # ASR和EAPC柱状图
│   ├── age_group_disparities.R           # 年龄组差异分析
│   ├── age_group_disparity_final.R       # 年龄组差异分析（最终版）
│   ├── age_group_analysis.R              # 年龄组差异分析
│   ├── arima_forecast.r                  # ARIMA预测分析
│   ├── disease_asr_asdr_eapc_correlation.R  # 疾病ASR、ASDR和EAPC相关性分析
│   ├── disease_proportion.R              # 疾病成分比分析
│   ├── specific_three_groups_incidence.r # 特定三组发病率分析
│   ├── specific_three_groups_dalys.r     # 特定三组DALYs分析
│   ├── un_population_preprocessing.r     # 联合国人口数据预处理
│   └── map_all.R                         # 全疾病地图
├── outcome/          # 输出结果
│   ├── ASR_SDI/        # SDI与ASR相关图
│   └── MAP/           # 地理分布图
├── docs/             # 文档目录
├── README.md         # 项目说明
└── 2025统计建模大赛/     # 比赛相关文件
```

## 3. 主要模块与功能

### 3.1 数据预处理模块

**文件**: `script/data_preprocessing.r`

**功能**:
- 解压并处理原始ZIP文件中的CSV数据
- 筛选特定年龄段（15-49岁）和指标（发病率、死亡率、DALYs）的数据
- 按指标类型分类保存处理后的数据

**关键函数**:
- 无特定函数，主要通过脚本流程处理

### 3.2 数据合并模块

**文件**: `script/merge_datasets.R`

**功能**:
- 合并不同疾病类别的数据
- 添加疾病来源标识
- 保存合并后的数据供后续分析使用

**关键函数**:
- `Datacombine(category, path_mapping)`: 合并指定类别文件夹内的所有CSV文件
  - **参数**:
    - `category`: 文件夹类别（需在path_mapping中定义）
    - `path_mapping`: 命名向量，定义类别与文件夹路径的映射
  - **返回值**: 合并后的数据框

### 3.3 SDI与发病率分析模块

**文件**: `script/sdi_incidence_correlation.R`

**功能**:
- 分析社会人口指数（SDI）与癌症发病率的关系
- 生成散点图和趋势线
- 组合多个图形并添加标签

**关键函数**:
- `generate_SDI_ASR(data, name)`: 生成SDI与ASR（年龄标准化率）的相关图
  - **参数**:
    - `data`: 输入数据
    - `name`: 输出文件名前缀
  - **返回值**: 包含Incidence和DALYS图形对象的列表
- `generate_legend(data, name)`: 生成共享图例
  - **参数**:
    - `data`: 输入数据
    - `name`: 输出文件名前缀
  - **返回值**: 图例对象

### 3.4 EAPC计算模块

**文件**: `script/eapc_functions.R`

**功能**:
- 计算癌症指标的年度百分比变化（EAPC）
- 生成包含1990年和2021年数据的对比表格
- 计算EAPC的置信区间

**关键函数**:
- `EAPC_count(age=8, measure, metric=1)`: 计算EAPC并生成结果表格
  - **参数**:
    - `age`: 年龄组ID
    - `measure`: 指标ID（1=死亡率，2=DALYs，6=发病率）
    - `metric`: 度量标准ID
  - **返回值**: 包含1990年和2021年数据及EAPC的结果表格

### 3.5 地理可视化模块

**文件**: `script/map_visualization.R`

**功能**:
- 生成全球癌症数据的地理分布图
- 包括发病率（ASR）、病例变化和EAPC的地图
- 处理国家名称映射，确保数据与地图匹配

**关键函数**:
- `generate_map(data, name)`: 生成三种类型的地图
  - **参数**:
    - `data`: 输入数据
    - `name`: 输出文件名前缀
  - **返回值**: 无，直接保存地图文件

### 3.6 其他分析模块

**文件**: `script/eapc_map_all.R`
**功能**: 处理所有疾病的EAPC数据，生成全球消化系统癌症的汇总表，并准备用于地图绘制的数据

**文件**: `script/eapc_table.R`
**功能**: 计算各疾病的EAPC数据，生成三线表，并导出到Excel文件

**文件**: `script/eapc_summary.R`
**功能**: 生成全球消化系统癌症的汇总表，包含各地区和各疾病的EAPC数据

**文件**: `script/asr_eapc_bar_chart.R`
**功能**: 生成不同疾病的ASR（年龄标准化率）和EAPC（年度百分比变化）的柱状图

**文件**: `script/age_group_disparities.R`
**功能**: 分析不同年龄组的疾病负担差异，特别是针对女性2021年的数据

**文件**: `script/age_group_disparity_final.R`
**功能**: 分析不同年龄组的疾病负担差异，生成年龄组分布和变化趋势图

**文件**: `script/age_group_analysis.R`
**功能**: 分析不同年龄组的疾病负担差异，生成年龄组分布图和百分比变化图

**文件**: `script/arima_forecast.r`
**功能**: 使用ARIMA模型对全球不同年龄组的DALYs数据进行预测（2022-2050年）

**文件**: `script/disease_asr_asdr_eapc_correlation.R`
**功能**: 分析癌症的ASR、ASDR与EAPC之间的相关性，以及HDI与EAPC之间的相关性

**文件**: `script/disease_proportion.R`
**功能**: 计算并可视化不同地区和年份的癌症发病率成分比

**文件**: `script/specific_three_groups_incidence.r`
**功能**: 处理不同癌症类型的发病率数据，对高SDI地区的数据进行分组和聚合

**文件**: `script/specific_three_groups_dalys.r`
**功能**: 处理不同癌症类型的DALYs数据，对低SDI地区的数据进行分组和聚合

**文件**: `script/un_population_preprocessing.r`
**功能**: 处理联合国人口数据，将宽格式数据转换为长格式，并添加年龄组名称和ID

**文件**: `script/map_all.R`
**功能**: 生成全疾病的地图可视化

## 4. 核心依赖

| 包名 | 用途 | 来源 |
|------|------|------|
| dplyr | 数据处理和操作 | CRAN |
| ggplot2 | 数据可视化 | CRAN |
| ggrepel | 防止标签重叠 | CRAN |
| cowplot | 图形组合和图例提取 | CRAN |
| patchwork | 图形组合 | CRAN |
| gridExtra | 图形布局 | CRAN |
| reshape | 数据重塑 | CRAN |
| maps | 地图数据 | CRAN |
| sf | 地理空间数据处理 | CRAN |
| readr | 数据读取 | CRAN |
| zip | 处理ZIP文件 | CRAN |
| tools | 文件路径处理 | CRAN |

## 5. 数据流程

1. **数据预处理**:
   - 从ZIP文件中提取原始数据
   - 筛选特定年龄段和指标的数据
   - 按指标类型分类保存

2. **数据合并**:
   - 合并不同疾病类别的数据
   - 添加疾病来源标识
   - 保存合并后的数据

3. **数据分析**:
   - 分析SDI与癌症发病率的关系
   - 计算EAPC评估癌症趋势

4. **可视化**:
   - 生成SDI与ASR的相关图
   - 生成全球地理分布图
   - 组合图形并添加标签

## 6. 关键数据文件

### 6.1 原始数据
- 按疾病类型分类存储在`data/raw/`目录的不同文件夹中
- 每个疾病文件夹包含多个CSV文件，分别对应不同的指标

### 6.2 中间数据
- 保存在`data/processed/`目录下
- 主要文件包括：
  - `original_data.Rdata`: 合并后的原始数据
  - `order.Rdata`: 包含国家排序信息
  - `SDI_clean.csv`: 清理后的社会人口指数数据

### 6.3 输出结果
- 图表: 保存在`outcome/`目录下，包括SDI与ASR相关图和地理分布图
- 分析结果: 保存在`data/processed/`目录下，包括各类统计分析结果

## 7. 运行指南

### 7.1 环境要求
- R 4.0+ 版本
- 安装所需的R包（见核心依赖部分）

### 7.2 运行顺序

1. **数据预处理**:
   - 运行 `script/data_preprocessing.r` 处理原始数据
   - 可根据需要修改脚本中的疾病文件夹路径（默认为`data/raw/肝`）

2. **数据合并**:
   - 运行 `script/merge_datasets.R` 合并不同疾病的数据
   - 生成 `data/processed/original_data.Rdata` 文件

3. **基础分析**:
   - 运行 `script/eapc_functions.R` 计算年度百分比变化
   - 运行 `script/disease_proportion.R` 计算疾病成分比
   - 运行 `script/disease_asr_asdr_eapc_correlation.R` 分析相关性

4. **年龄组分析**:
   - 运行 `script/age_group_analysis.R` 分析年龄组差异
   - 运行 `script/age_group_disparities.R` 分析女性年龄组差异
   - 运行 `script/age_group_disparity_final.R` 生成最终年龄组分析

5. **SDI相关性分析**:
   - 运行 `script/sdi_incidence_correlation.R` 分析SDI与癌症发病率的关系

6. **地理可视化**:
   - 运行 `script/map_visualization.R` 生成单个疾病的地图
   - 运行 `script/map_all.R` 生成全疾病的地图
   - 运行 `script/eapc_map_all.R` 生成EAPC相关地图

7. **汇总与表格**:
   - 运行 `script/eapc_table.R` 生成EAPC三线表
   - 运行 `script/eapc_summary.R` 生成全球汇总表
   - 运行 `script/asr_eapc_bar_chart.R` 生成ASR和EAPC柱状图

8. **预测分析**:
   - 运行 `script/arima_forecast.r` 使用ARIMA模型进行预测

9. **特定分析**:
   - 运行 `script/specific_three_groups_incidence.r` 分析高SDI地区发病率
   - 运行 `script/specific_three_groups_dalys.r` 分析低SDI地区DALYs
   - 运行 `script/un_population_preprocessing.r` 处理联合国人口数据

### 7.3 注意事项
- 确保工作目录设置正确，大多数脚本会自动设置为项目根目录
- 确保所有依赖包已安装（可使用 `install.packages()` 安装缺失的包）
- 原始数据文件结构需与脚本期望的一致，确保 `data/raw/` 目录下有正确的疾病文件夹
- 运行脚本时，建议按照上述顺序执行，因为后续脚本可能依赖前面脚本的输出
- 部分脚本可能需要较长时间运行，特别是涉及全球数据处理和地图生成的脚本

### 7.4 输出结果
- **数据文件**:
  - 处理后的数据保存在 `data/processed/` 目录
  - 包括合并后的数据集、EAPC计算结果、汇总表等

- **图表**:
  - SDI与ASR相关图保存在 `outcome/ASR_SDI/` 目录
  - 地理分布图保存在 `outcome/MAP/` 目录
  - 其他图表保存在 `outcome/` 目录根目录

- **Excel文件**:
  - 汇总表和分析结果导出为Excel文件，保存在 `data/processed/` 目录

## 8. 项目亮点

1. **多维度分析**: 从发病率、死亡率和DALYs三个维度分析癌症负担
2. **地理可视化**: 通过世界地图直观展示全球癌症负担的空间分布
3. **趋势分析**: 使用EAPC指标评估癌症趋势的变化
4. **相关性分析**: 探讨SDI与癌症发病率的关系，为政策制定提供参考
5. **模块化设计**: 代码结构清晰，功能模块化，便于维护和扩展

## 9. 后续建议

1. **数据更新**: 定期更新数据，保持分析的时效性
2. **模型优化**: 考虑使用更复杂的统计模型，如混合效应模型
3. **预测分析**: 基于历史数据，预测未来癌症负担趋势
4. **交互可视化**: 开发交互式仪表板，提高数据探索的效率
5. **多因素分析**: 考虑更多影响因素，如环境、生活方式等

## 10. 结论

本项目通过系统化的数据分析和可视化，全面展示了全球消化系统癌症的流行趋势和地理分布。研究结果有助于了解癌症负担的变化趋势，为癌症防控策略的制定提供科学依据。项目代码结构清晰，功能模块化，为后续的扩展和深入研究奠定了基础。
