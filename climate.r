install.packages("purrr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("https://meteoexploration.com/R/insol/insol_1.2.2.tar.gz", repos=NULL, type="source")

library(purrr)
library(dplyr)
library(lubridate)
library(insol)
library(tidyr)

# ---- 合并具有共同命名模式的文件 ----

# 设置工作目录
setwd("C:/Users/LENOVO/Desktop/phephy/ERA")

# 列出所有以"_climate_data.csv"结尾的文件
file_list <- list.files(pattern = "_climate_data\\.csv$")

# 读取所有这些文件并创建数据框列表
data_list <- lapply(file_list, read.csv)

# 合并所有数据框
combined_data <- do.call(rbind, data_list)

# 写入新的CSV文件
write.csv(combined_data, "climate_data.csv", row.names = FALSE)



# ---- 气候数据转化 ----

# 读取数据，这个在上面也有了
combined_data <- read.csv("C:/Users/LENOVO/Desktop/phephy/ERA/climate_data.csv")

# 数据来源：https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_AGGR#bands

# 定义一个函数来找到首次连续出现6个1的位置
find_first_sequence_of_6 <- function(x) {
  # 寻找连续的1
  with(rle(x), {
    # 找到第一个长度至少为6的序列
    idx <- which(values == 1 & lengths >= 6)
    if (length(idx) > 0) {
      # 返回这个序列开始的位置
      sum(lengths[1:(idx[1]-1)]) + 1
    } else {
      # 如果没有找到，返回NA
      NA
    }
  })
}

data_climate <- combined_data %>%
  # 将 temperature 转换为摄氏度
  mutate(temperature = temperature - 273.15) %>%
  # 根据新的 temperature 计算 tmp_CU 和 tmp_FU，每天的累加值
  mutate(tmp_CU = ifelse(temperature < 5, 1, 0),
         tmp_FU1 = ifelse(temperature >= 5, 1, 0),
         tmp_FU = ifelse(temperature >= 5, temperature - 5, 0)) %>%
  # 将 precipitation 和 evaporation 转换为毫米
  mutate(precipitation = precipitation * 1000,
         evaporation = evaporation * 1000) %>%
  # 将 date 列分割成 YEAR 和 DAY 列
  mutate(YEAR = year(ymd(date)),
         DAY = yday(ymd(date))) %>%
  # 选择并重新排列列
  select(PEP_ID, temperature, tmp_CU, tmp_FU1, tmp_FU, precipitation, evaporation, thermal, solar, YEAR, DAY)

# 加上 TSOS
data_climate <- data_climate %>%
  group_by(PEP_ID, YEAR) %>%
  mutate(TSOS = find_first_sequence_of_6(tmp_FU1)) %>%
  ungroup()

# head(data_climate)
# tail(data_climate)

# ---- 计算气候的累加值 ----

# 加上各个的 sum
climate_data1 <- data_climate %>%
  arrange(PEP_ID, YEAR, DAY) %>%  # 确保首先按照PEP_ID，然后是YEAR和DAY排序
  group_by(PEP_ID, YEAR) %>%  # 按照PEP_ID和YEAR进行分组
  mutate(
    tmp_CU = replace_na(tmp_CU, 0),
    tmp_FU1 = replace_na(tmp_FU1, 0),
    tmp_FU = replace_na(tmp_FU, 0),
    precipitation = replace_na(precipitation, 0),
    evaporation = replace_na(evaporation, 0),
    thermal = replace_na(thermal, 0),
    solar = replace_na(solar, 0)
  ) %>%
  mutate(
    CU_sum = cumsum(tmp_CU),
    FU_sum = cumsum(tmp_FU),
    pre_sum = cumsum(precipitation),
    eva_sum = cumsum(evaporation),
    ther_sum = cumsum(thermal),
    sol_sum = cumsum(solar)
  ) %>%
  ungroup()  # 移除分组，使得后续的操作不受分组影响

# head(climate_data1,30)
# tail(climate_data1,30)


# 将修改后的数据写入新的CSV文件
write.csv(climate_data1, "C:/Users/LENOVO/Desktop/phephy/ERA/climate_data1.csv", row.names = FALSE)



# ---- 计算最终的气候 ----

# 读取数据
# climate_data1 <- read.csv("C:/Users/LENOVO/Desktop/phephy/ERA/climate_data1.csv")

# ---- 计算 FU（从2.1开始计算的，错了） ----
# 直接用 前面的sum就可以了
# data_with_fu <- data %>%
#   group_by(PEP_ID, YEAR) %>%
#   mutate(FU = if_else(DAY >= 32, cumsum(tmp_FU) - cumsum(tmp_FU)[DAY == 32] + tmp_FU[DAY == 32], 0)) %>%
#   ungroup()

# ---- pre eva 从TSOS开始计算 ----
# climate_data_all <- final_data_with_cu %>%
#   group_by(PEP_ID, YEAR) %>%
#   mutate(
#     pre = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, precipitation, 0)), 0),
#     eva = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, evaporation, 0)), 0),
#     ther = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, thermal, 0)), 0),
#     sol = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, solar, 0)), 0)
#   ) %>%
#   ungroup()
# ---- 计算CU ----

# 计算每个 PEP_ID 和 YEAR 的 DAY=305 到年末的 tmp_CU 累加值
year_end_cu <- climate_data1 %>%
  filter(DAY >= 305) %>%
  group_by(PEP_ID, YEAR) %>%
  summarise(CU_pre = sum(tmp_CU, na.rm = TRUE)) %>%
  ungroup()

# 将这个累加值分配给下一年
year_end_cu <- year_end_cu %>%
  mutate(YEAR = YEAR + 1)

# 合并到原始数据框中
climate_data_all <- climate_data1 %>%
  left_join(year_end_cu, by = c("PEP_ID", "YEAR"))

# head(climate_data_all)

# 将修改后的数据写入新的CSV文件
# write.csv(climate_data_all, "C:/Users/LENOVO/Desktop/phephy/ERA/climate_data_all.csv", row.names = FALSE)

# ---- 计算光周期 ----

# 读取env_566.csv文件
env_data <- read.csv("C:/Users/LENOVO/Desktop/phephy/ERA/env.csv")

# 根据PEP_ID合并数据框
climate_data_all <- left_join(climate_data_all, env_data, by = "PEP_ID")

# 假设 climate_data_all 是你的数据框，YEAR 和 DAY 列已经存在
climate_data_all <- climate_data_all %>%
  mutate(
    DATE = as.Date(paste(YEAR, DAY - 1, sep="-"), "%Y-%j") # 使用%j来指定一年中的第几天
  )

# 使用 daylength 函数计算日照时长
climate_data_all1 <- climate_data_all %>%
  rowwise() %>%
  mutate(daylen = {
    # 调用 daylength 函数
    dl <- daylength(lat = latitude, long = longitude, jd = DAY, tmz = 1)
    # 从返回的列表中提取 daylen
    dl[3]
  }) %>%
  ungroup()

# 查看结果
head(climate_data_all1)

climate_data_all <- climate_data_all1

write.csv(climate_data_all, "C:/Users/LENOVO/Desktop/phephy/ERA/climate_data_all.csv", row.names = FALSE)


# ---- 最终的数据 ----

# 简化数据框并更新 CU_sum
data_climate <- climate_data_all %>%
  mutate(CU_sum = CU_sum + coalesce(CU_pre, 0)) %>%  # 更新 CU_sum，使用 coalesce 处理 NA 值
  select(PEP_ID, YEAR, DAY, TSOS, CU_sum, FU_sum, pre_sum, eva_sum, ther_sum, sol_sum, daylen)  # 选择保留的列

# 重命名列
data_climate <- data_climate %>%
  rename(
    CU = CU_sum,
    FU = FU_sum,
    pre = pre_sum,
    eva = eva_sum,
    ther = ther_sum,
    sol = sol_sum,
    pho = daylen
  )

head(data_climate)

write.csv(data_climate, "C:/Users/LENOVO/Desktop/phephy/ERA/data_climate.csv", row.names = FALSE)
