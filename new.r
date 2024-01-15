# 安装和加载必要的包
install.packages("ggplot2")
install.packages("maps")

library(ggplot2)
library(maps)

# 设置工作目录，将路径更改为包含文件的目录
setwd("C:/Users/LENOVO/Desktop/lab/phylogeny")

# 读取文件
file_path <- "st BBCH 11 leaf.csv"  # 文件名
data <- read.csv(file_path, header = TRUE)  # 读取CSV文件，如果是其他格式，请适当修改

# 查看数据结构
str(data)

# 打印数据的前几行
head(data)




#* *每个PEP_ID 对应一个样点，总共有7795个包含 BBCH11的样点*

# 获取唯一的 PEP_ID 值
unique_PEP_IDs <- unique(data$PEP_ID)

# 根据每个 PEP_ID 创建一个文件
for (id in unique_PEP_IDs) {
  # 选择当前 PEP_ID 的数据
  subset_data <- data[data$PEP_ID == id, ]
  
  # 构建文件名
  file_name <- paste0(id, "_BBCH_11.csv")
  
  # 保存数据到CSV文件
  write.csv(subset_data, file_name, row.names = FALSE)
  
  cat("File", file_name, "created.\n")
}

# 打印唯一 PEP_ID 的数量
cat("Total number of unique PEP_IDs:", length(unique_PEP_IDs), "\n")

# 找到LON和LAT的最小值和最大值
min_lon <- min(data$LON)
max_lon <- max(data$LON)
min_lat <- min(data$LAT)
max_lat <- max(data$LAT)

# 打印结果
cat("Min LON:", min_lon, "\n")
cat("Max LON:", max_lon, "\n")
cat("Min LAT:", min_lat, "\n")
cat("Max LAT:", max_lat, "\n")





#* * 将每个 PEP_ID 对应的经纬度信息映射到欧洲地图上 *

# 创建一个数据框存储每个PEP_ID对应的经纬度和数据量
df <- data.frame(lon = data$LON, lat = data$LAT, count = 1)

# 根据数据量排序数据框
df <- df[order(df$count, decreasing = TRUE), ]

# 计算点的大小
df$size <- sqrt(1:nrow(df)) * 2


# 过滤掉缺失经纬度的数据
data <- data %>% filter(!is.na(LON) & !is.na(LAT))
http://127.0.0.1:25953/graphics/plot_zoom_png?width=1536&height=814
# 对每个 PEP_ID 计算数据量
data_counts <- aggregate(cbind(COUNT = LON) ~ PEP_ID + LON + LAT, data, length)

# 加载世界地图
world <- map_data("world")
world <- subset(world, lat > 25 & long > -10 & long < 40)
# world <- subset(world, lat > 40 & lat < 60 & long > 5 & long < 20)

# 绘制地图和数据点
ggplot() +  
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "white", color = "black") +  
  geom_point(data = data_counts, aes(x = LON, y = LAT, size = 0.05, color = COUNT), alpha = 0.7) +
  # geom_point(data = subset(data_counts, LAT > 40 & LAT < 60 & LON > 5 & LON < 20), 
             # aes(x = LON, y = LAT, size = 1, color = COUNT), alpha = 0.7) +  
  scale_color_gradient(low = "pink", high = "darkred") +  
  theme_minimal() +  
  labs(title = "PEP_ID Data Points on Europe Map", x = "Longitude", y = "Latitude")


