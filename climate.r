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






# ---- 将气候数据转化格式 ----

install.packages("dplyr")
install.packages("lubridate")
library(purrr)
library(dplyr)
library(lubridate)

# 读取数据
data <- read.csv("C:/Users/LENOVO/Desktop/phephy/ERA/climate_data.csv")

# 数据来源：https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_AGGR#bands

# 定义一个函数来找到首次连续出现5个1的位置
find_first_sequence_of_5 <- function(x) {
  # 寻找连续的1
  with(rle(x), {
    # 找到第一个长度至少为5的序列
    idx <- which(values == 1 & lengths >= 5)
    if (length(idx) > 0) {
      # 返回这个序列开始的位置
      sum(lengths[1:(idx[1]-1)]) + 1
    } else {
      # 如果没有找到，返回NA
      NA
    }
  })
}

data_climate <- data %>%
  # 将 temperature 转换为摄氏度
  mutate(temperature = temperature - 273.15) %>%
  # 根据新的 temperature 计算 tmp_CU 和 tmp_FU
  mutate(tmp_CU = ifelse(temperature < 5, 1, 0),
         tmp_FU = ifelse(temperature >= 5, 1, 0)) %>%
  # 将 precipitation 和 evaporation 转换为毫米
  mutate(precipitation = precipitation * 1000,
         evaporation = evaporation * 1000) %>%
  # 将 date 列分割成 YEAR 和 DAY 列
  mutate(YEAR = year(ymd(date)),
         DAY = yday(ymd(date))) %>%
  # 选择并重新排列列
  select(PEP_ID, temperature, tmp_CU, tmp_FU, precipitation, evaporation, thermal, solar, YEAR, DAY)

# 加上 TSOS
data_climate <- data_climate %>%
  group_by(PEP_ID, YEAR) %>%
  mutate(TSOS = find_first_sequence_of_5(tmp_FU)) %>%
  ungroup()

head(data_climate)
tail(data_climate)

# 加上各个的 sum
data_sum <- data_climate %>%
  arrange(PEP_ID, YEAR, DAY) %>%  # 确保首先按照PEP_ID，然后是YEAR和DAY排序
  group_by(PEP_ID, YEAR) %>%  # 按照PEP_ID和YEAR进行分组
  mutate(
    tmp_CU = replace_na(tmp_CU, 0),
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

head(data_sum,30)
tail(data_sum,30)


# 将修改后的数据写入新的CSV文件
write.csv(data_sum, "C:/Users/LENOVO/Desktop/phephy/ERA/climate_data1.csv", row.names = FALSE)






# ---- 得到我需要的气象数据 ----

library(dplyr)

# 读取数据
data <- read.csv("C:/Users/LENOVO/Desktop/phephy/ERA/climate_data1.csv")

# ---- 计算 FU ----
data_with_fu <- data %>%
  group_by(PEP_ID, YEAR) %>%
  mutate(FU = if_else(DAY >= 32, cumsum(tmp_FU) - cumsum(tmp_FU)[DAY == 32] + tmp_FU[DAY == 32], 0)) %>%
  ungroup()

# ---- 计算CU ----
# 计算每个 PEP_ID 每个 YEAR DAY>=305 的 tmp_CU 累积，这个数值对于每一YEAR来说都是定值
previous_year_end_cu <- data_with_fu %>%
  filter(DAY >= 305) %>%
  group_by(PEP_ID, YEAR) %>%
  summarise(end_year_CU = sum(tmp_CU)) %>%
  mutate(YEAR = YEAR + 1) %>%
  ungroup()

# 将前一年的累积值合并回数据集
data_with_end_cu <- data_with_fu %>%
  left_join(previous_year_end_cu, by = c("PEP_ID", "YEAR"))

# 计算 CU
final_data_with_cu <- data_with_end_cu %>%
  group_by(PEP_ID, YEAR) %>%
  mutate(CU = if_else(DAY <= 304,
                      cumsum(tmp_CU) + coalesce(end_year_CU, 0),
                      cumsum(tmp_CU) - sum(if_else(DAY < 305, tmp_CU, 0), na.rm = TRUE))) %>%
  ungroup()

# ---- 计算pre eva ----
final_data_with_all <- final_data_with_cu %>%
  group_by(PEP_ID, YEAR) %>%
  mutate(
    pre = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, precipitation, 0)), 0),
    eva = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, evaporation, 0)), 0),
    ther = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, thermal, 0)), 0),
    sol = if_else(DAY >= TSOS, cumsum(if_else(DAY >= TSOS, solar, 0)), 0)
  ) %>%
  ungroup()

head(final_data_with_all)

# 将修改后的数据写入新的CSV文件
write.csv(final_data_with_all, "C:/Users/LENOVO/Desktop/phephy/ERA/climate_data_all.csv", row.names = FALSE)

