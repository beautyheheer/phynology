# ---- 所有BBCH 绘图 ----

library(ggplot2)
library(dplyr)
library(readr)

# 设置工作目录到包含CSV文件的文件夹
setwd("C:/Users/LENOVO/Desktop/PEP/split_by_BBCH")

# 获取文件夹中的所有CSV文件名
file_list <- list.files(pattern = "*.csv")

# 循环处理每个文件
for (file in file_list) {
  # 读取文件
  data <- read_csv(file)
  
  # 提取BBCH编号
  bbch_number <- gsub("BBCH_(\\d+).csv", "\\1", file)
  
  # 计算每个树种每年的数据点数量
  species_year_counts <- data %>%
    group_by(treetype, YEAR) %>%
    summarise(Data = n(), .groups = 'drop')
  
  # 创建图表
  plot <- ggplot(species_year_counts, aes(x = YEAR, y = treetype, size = Data, color = Data)) +
    geom_point(shape = 20, stroke = 0) +
    theme_light() +
    labs(x = "Year", y = "Tree Type", size = "Data Count", color = "Data Count") +
    ggtitle(paste("Data Count for BBCH", bbch_number)) +
    scale_size_continuous(range = c(2, 4)) +
    scale_color_gradient(low = "#FF779F", high = "#E03167")
  
  
  # 保存图表为JPG格式
  ggsave(paste0("BBCH_", bbch_number, ".jpg"), plot, device = "jpg", width = 15, height = 10)
}











# ---- BBCH11 的时间跨度 ----

# 设置工作目录
setwd("C:/Users/LENOVO/Desktop/PEP/split_by_BBCH")

# 获取文件夹中的所有文件名
file_list <- list.files()

# 创建一个空的数据框，用于存储每个物种的数据
species_timespan <- data.frame(Species = character(), Year = numeric(), Data = numeric())

# 循环读取每个文件
for (file in file_list) {
  # 读取文件
  data <- read.csv(file)
  
  # 提取物种名称
  species_name <- gsub("_BBCH_11.csv", "", file)
  
  # 提取年份
  years <- unique(data$YEAR)
  
  # 循环检查每个年份是否有数据点
  for (year in years) {
    data_count <- sum(data$YEAR == year)
    # 将结果添加到数据框中
    species_timespan <- rbind(species_timespan, data.frame(Species = species_name, Year = year, Data = data_count))
  }
}

# 加载所需的包
library(ggplot2)

# 获取所有年份并按照从早到晚的顺序排列
all_years <- unique(species_timespan$Year)
all_years <- all_years[order(all_years)]

library(dplyr)

# 计算每个树种的 YEAR 值范围
species_range <- species_timespan %>%
  group_by(Species) %>%
  summarise(year_min = min(Year), year_max = max(Year)) %>%
  mutate(label = paste(year_min, "-", year_max, sep = ""))

# 创建图表
plot <- ggplot(species_timespan, aes(x = Year, y = Species)) +
  geom_point(aes(size = 1, color = cut(Data, breaks = c(-Inf, 3, 9, Inf))), shape = 20, stroke = 0) +
  scale_color_manual(values = c("#FEE5EA", "#FF779F", "#E03167"), labels = c("1-3", "4-9", ">9")) +
  theme_light() +
  labs(x = "Year", y = "", color = "Data Count") +
  ggtitle("Data Count for Each Species Over Time") +
  scale_x_continuous(breaks = seq(1925, 2017, by = 5)) +
  geom_text(data = species_range, size = 2.5,color = "#949EC5",aes(x = 1937, y = Species, label = label), hjust = -0.1)

# 保存为PNG格式
ggsave("C:/Users/LENOVO/Desktop/lab/phylogeny/photo/species_timespan1.png", plot, device = "png", width = 15, height = 10)

# 保存为JPG格式
ggsave("C:/Users/LENOVO/Desktop/lab/phylogeny/photojpg/species_timespan1.jpg", plot, device = "jpg", width = 15, height = 10)