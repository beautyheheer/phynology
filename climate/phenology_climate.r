library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(mgcv)
library(gridExtra)

# ---- 物候数据和气象数据融合 ----

# 设置工作目录
setwd("C:/Users/LENOVO/Desktop/phephy/data")

# 读取数据
data_phenology <- read.csv("C:/Users/LENOVO/Desktop/phephy/data/data_50y_filtered.csv", stringsAsFactors = FALSE)
data_climate <- read.csv("C:/Users/LENOVO/Desktop/phephy/ERA/data_climate.csv", stringsAsFactors = FALSE)

# 融合数据
data <- merge(data_phenology, data_climate, by.x = c("PEP_ID", "YEAR", "DOY"), by.y = c("PEP_ID", "YEAR", "DAY"))

# 查看结果
head(data)

# 保存新文件
write.csv(data, "C:/Users/LENOVO/Desktop/phephy/data/data.csv", row.names = FALSE)

# ---- 物候数据和气象数据融合 ----

library(dplyr)

# 步骤1: 读取数据
data_Alnus_glutinosa <- read.csv("C:/Users/LENOVO/Desktop/phephy/leaf/data_Alnus_glutinosa.csv")
data_climate <- read.csv("C:/Users/LENOVO/Desktop/phephy/ERA/data_climate.csv")

# 步骤2: 匹配数据行
matched_data <- inner_join(data_Alnus_glutinosa, data_climate, by = c("PEP_ID", "YEAR", "DAY"))

# 保存匹配后的数据框
# write.csv(matched_data, "C:/Users/LENOVO/Desktop/phephy/leaf/matched_data.csv", row.names = FALSE)

# 步骤3: 清理含有NA的行
clean_data <- na.omit(matched_data)

# 保存清理后的数据框
write.csv(clean_data, "C:/Users/LENOVO/Desktop/phephy/leaf/clean_data.csv", row.names = FALSE)


# ---- 存在DOY早于TSOS的样点 ----

# 找出pre为0的所有不同的PEP_ID
pep_ids_with_pre_zero <- merged_data %>%
  filter(pre == 0) %>%  # 筛选出pre为0的行
  distinct(PEP_ID)      # 选择不重复的PEP_ID

# 查看结果
print(pep_ids_with_pre_zero)

# 提取PEP_ID列作为向量
pep_ids_vector <- pep_ids_with_pre_zero$PEP_ID

# 查看向量
print(pep_ids_vector)



# ---- 点映射到地图上 ----

# 读取stations数据
stations_data <- read.csv("C:/Users/LENOVO/Desktop/phephy/data/stations.csv", 
                          header = TRUE, stringsAsFactors = FALSE)

# 假设你已经有了pep_ids_with_pre_zero向量
selected_stations <- stations_data[stations_data$PEP_ID %in% pep_ids_with_pre_zero$PEP_ID, ]

head(selected_stations)

# 创建一个地图并添加点，同时限制显示范围
ggplot() +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "gray80", color = "gray70") +
  geom_point(data = selected_stations, aes(x = LON, y = LAT), color = "red") +
  coord_cartesian(xlim = c(5.7, 17), ylim = c(46, 55.5)) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", title = "Selected Stations")


# ---- 趋势转折点判断 ----



# ---- 平滑的曲线拟合（一个树种） ----

# 筛选Quercus_robur树种的数据
quercus_data <- data %>%
  filter(treetype == "Quercus_robur")

# 首先计算每个PEP_ID每年的均值
quercus_data_avg <- quercus_data %>%
  group_by(PEP_ID, YEAR) %>%
  summarise(AVG_DOY = mean(DOY, na.rm = TRUE)) %>%
  ungroup()

# 然后使用ggplot2绘制每个PEP_ID的趋势，并使用geom_smooth进行平滑拟合
ggplot(quercus_data_avg, aes(x = YEAR, y = AVG_DOY)) +
  geom_line(aes(group = PEP_ID), color = "grey") + # 使用灰色线条表示每个PEP_ID的趋势
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "blue") + # 使用蓝色线条表示总体趋势的平滑拟合
  theme_minimal() +
  labs(x = "Year", y = "Average Day of Year (DOY)", title = "Trend of Average DOY by PEP_ID") +
  theme(legend.position = "none") # 隐藏图例


# ---- 平滑的曲线拟合（六个树种） ----

# 首先计算每个PEP_ID每年的均值，这些将用于绘制灰色线条
data_avg <- data %>%
  group_by(PEP_ID, YEAR) %>%
  summarise(AVG_DOY = mean(DOY, na.rm = TRUE)) %>%
  ungroup()

# 然后计算每个treetype每年的均值和标准差，这些将用于绘制彩色平滑曲线和阴影
data_summary <- data %>%
  group_by(treetype, YEAR) %>%
  summarise(
    AVG_DOY = mean(DOY, na.rm = TRUE),
    SD_DOY = sd(DOY, na.rm = TRUE)
  ) %>%
  ungroup()

# 绘制图表 → 六个小图，每个灰色线+平滑的彩色线
ggplot() +
  geom_line(data = data_avg, aes(x = YEAR, y = AVG_DOY, group = PEP_ID), color = "grey", alpha = 0.8) +
  geom_smooth(data = data_summary, aes(x = YEAR, y = AVG_DOY, fill = treetype, color = treetype), 
              method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_brewer(palette = "Set1") + # 为平滑曲线指定颜色
  scale_fill_brewer(palette = "Set1") + # 为阴影区域指定颜色
  facet_wrap(~treetype, scales = "free_y") +
  theme_minimal() +
  labs(x = "Year", y = "DOY", title = "DOY-YEAR") +
  theme(legend.position = "bottom") # 将图例放置在底部

# 绘制图表 → 六个小图，每个灰色线+平均值线
ggplot() +
  geom_line(data = data_avg, aes(x = YEAR, y = AVG_DOY, group = PEP_ID), color = "grey", alpha = 0.8) +
  geom_line(data = data_summary, aes(x = YEAR, y = AVG_DOY, color = treetype, group = treetype), size = 1) +
  geom_ribbon(data = data_summary, aes(x = YEAR, ymin = AVG_DOY - SD_DOY, ymax = AVG_DOY + SD_DOY, fill = treetype), alpha = 0.2) +
  scale_color_brewer(palette = "Set1") + # 为线条指定颜色
  scale_fill_brewer(palette = "Set1") + # 为阴影区域指定颜色
  facet_wrap(~treetype, scales = "free_y") +
  theme_minimal() +
  labs(x = "Year", y = "DOY", title = "DOY-YEAR") +
  theme(legend.position = "bottom") # 将图例放置在底部


# 绘制所有treetype的平滑曲线和阴影区域在一个图中
ggplot(data_summary, aes(x = YEAR, y = AVG_DOY, group = treetype, color = treetype, fill = treetype)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) + # 添加平滑曲线和阴影区域
  scale_color_brewer(palette = "Set1") + # 为平滑曲线指定颜色
  scale_fill_brewer(palette = "Set1") + # 为阴影区域指定颜色
  theme_minimal() +
  labs(x = "Year", y = "DOY", title = "DOY-YEAR") +
  theme(legend.position = "bottom") # 将图例放置在底部

