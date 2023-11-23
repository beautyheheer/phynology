# 【绘图 所有的】
library(ggplot2)

# 读取 DE_11.CSV 文件
file_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/DE_11.csv"
data <- read.csv(file_path, header = TRUE)

# 计算每个物种每年的 DAY 的平均值
data_summary <- aggregate(DAY ~ treetype + YEAR, data = data, FUN = mean)

# 绘制趋势图
ggplot(data = data_summary, aes(x = YEAR, y = DAY, group = treetype, color = treetype)) +
  geom_line(size = 1) +  # 设置线条粗细为 1
  labs(x = "Year", y = "Day", title = "Day-Year Trend for each treetype") +
  theme_minimal() + # 设定图表的主题为 minimal，以简洁的风格呈现图表
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_color_brewer(palette = "Set1") + # 更改颜色，使用不同颜色表示不同的物种
  scale_x_continuous(breaks = seq(min(data_summary$YEAR), max(data_summary$YEAR), by = 2))  # 设置横坐标刻度



# 【绘图 前6的】
library(ggplot2)

# 读取 DE_11.CSV 文件
file_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/DE_11.csv"
data <- read.csv(file_path, header = TRUE)

# 计算每个物种的总数据量
species_counts <- as.data.frame(table(data$treetype))
colnames(species_counts) <- c("treetype", "Count")

# 按数据量降序排列
species_counts <- species_counts[order(-species_counts$Count), ]

# 获取排名前6的物种
top_species <- head(species_counts$treetype, 6)

# 从数据中筛选出排名前6的物种数据
filtered_data <- subset(data, treetype %in% top_species)

# 计算每个物种每年的 DAY 的平均值
data_summary <- aggregate(DAY ~ treetype + YEAR, data = filtered_data, FUN = mean)

# 绘制趋势图
ggplot(data = data_summary, aes(x = YEAR, y = DAY, group = treetype, color = treetype)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Day", title = "Day-Year Trend for Top 6 treetypes") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(min(data_summary$YEAR), max(data_summary$YEAR), by = 2))







# 【去趋势 原始折线图 lm图 去趋势后 都显示】
library(ggplot2)

# 读取 DE_11.CSV 文件
file_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/DE_11.csv"
data <- read.csv(file_path, header = TRUE)

# 计算每个物种的总数据量
species_counts <- as.data.frame(table(data$treetype))
colnames(species_counts) <- c("treetype", "Count")

# 按数据量降序排列
species_counts <- species_counts[order(-species_counts$Count), ]

# 获取排名前6的物种
top_species <- head(species_counts$treetype, 6)

# 从数据中筛选出排名前6的物种数据
filtered_data <- subset(data, treetype %in% top_species)

# 计算每个物种每年的 DAY 的平均值
data_summary <- aggregate(DAY ~ treetype + YEAR, data = filtered_data, FUN = mean)

# 绘制趋势图
ggplot(data = data_summary, aes(x = YEAR, y = DAY, group = treetype, color = treetype)) +
  geom_line(linewidth = 1) +
  labs(x = "Year", y = "Day", title = "Day-Year Trend for Top 6 treetypes") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(min(data_summary$YEAR), max(data_summary$YEAR), by = 2)) +
  
  # 去除趋势
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +  # 使用 LOESS 方法拟合趋势线
  geom_point(data = data_summary, aes(x = YEAR, y = DAY / fitted(loess(DAY ~ YEAR, data = data_summary))), color = "black", linewidth = 2)  # 绘制去趋势后的点图






# 【只显示去趋势化的】
library(ggplot2)

# 读取 DE_11.CSV 文件
file_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/DE_11.csv"
data <- read.csv(file_path, header = TRUE)

# 计算每个物种的总数据量
species_counts <- as.data.frame(table(data$treetype))
colnames(species_counts) <- c("treetype", "Count")

# 按数据量降序排列
species_counts <- species_counts[order(-species_counts$Count), ]

# 获取排名前6的物种
top_species <- head(species_counts$treetype, 6)

# 从数据中筛选出排名前6的物种数据
filtered_data <- subset(data, treetype %in% top_species)

# 计算每个物种每年的 DAY 的平均值
data_summary <- aggregate(DAY ~ treetype + YEAR, data = filtered_data, FUN = mean)

# 计算去趋势后的值
loess_fit <- loess(DAY ~ YEAR, data = data_summary)  # 使用 LOESS 拟合 DAY 和 YEAR 之间的关系
data_summary$detrended <- data_summary$DAY / predict(loess_fit)  # 计算去趋势后的值，即将原始 DAY 列除以 LOESS 拟合的值

# 绘制去趋势后的折线图
ggplot(data = data_summary, aes(x = YEAR, y = detrended, group = treetype, color = treetype)) +  # 设置数据和美学映射
  geom_line(size = 1) +  # 绘制折线图
  labs(x = "Year", y = "Detrended Day", title = "Detrended Day-Year Trend for Top 6 treetypes") +  # 设置图表标题和轴标签
  theme_minimal() +  # 使用最小化的主题风格
  theme(legend.position = "right", legend.title = element_blank()) +  # 设置图例位置和标题
  scale_color_brewer(palette = "Set1") +  # 设置颜色
  scale_x_continuous(breaks = seq(min(data_summary$YEAR), max(data_summary$YEAR), by = 2))  # 设置 X 轴刻度

