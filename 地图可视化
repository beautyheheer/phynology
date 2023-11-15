#【是一段啥也没做出来的代码 哈哈哈！下次再改】
# 导入所需的包
library(ggplot2)
library(dplyr)

# 读取数据文件
data <- read.csv("C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/st BBCH 13 leaf 0.5.csv", header = TRUE)

# 计算每个 PEP_ID 的数据量
data_count <- data %>%
  group_by(PEP_ID) %>%
  summarize(count = n())

# 合并数据
data_merged <- merge(data, data_count, by = "PEP_ID")

# 获取地图
map <- get_googlemap(center = c(lon = mean(data_merged$LON), lat = mean(data_merged$LAT)), zoom = 10, scale = 2)

# 绘制地图
ggmap(map) +
  geom_point(data = data_merged, aes(x = LON, y = LAT, size = count, color = "red")) +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "PEP_ID Data Map") +
  theme_minimal()
