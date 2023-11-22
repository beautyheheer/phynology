#【地图！！】
install.packages("maps")

library(ggplot2)
library(maps)

# 读取CSV文件
data <- read.csv("C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/DE_11.csv")

world = map_data("world")

world = subset(world, lat > 47.3 & lat < 55.2 & long > 5.5 & long < 15.2)

data_pos = data[,c("LON","LAT")]
data_pos = unique(data_pos)
data_pos = round(data_pos,digits = 1)
data_pos = unique(data_pos)


ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "white", fill = "grey", size = 0.1) +
  
  geom_point(data = data_pos,
             aes(x = LON, y = LAT ),shape=21)
