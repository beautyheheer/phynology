# 安装和加载必要的包
install.packages("ggplot2")
install.packages("maps")
install.packages("dplyr")

library(ggplot2)
library(maps)
library(dplyr)

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
# for (id in unique_PEP_IDs) {
#   # 选择当前 PEP_ID 的数据
#   subset_data <- data[data$PEP_ID == id, ]
# 
#   # 构建文件名
#   file_name <- paste0(id, "_BBCH_11.csv")
# 
#   # 保存数据到CSV文件
#   write.csv(subset_data, file_name, row.names = FALSE)
# 
#   cat("File", file_name, "created.\n")
# }

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

# 对每个 PEP_ID 计算数据量
data_counts <- aggregate(cbind(COUNT = LON) ~ PEP_ID + LON + LAT, data, length)

# 加载世界地图
world <- map_data("world")
world <- subset(world, lat > 25 & long > -10 & long < 40)
# world <- subset(world, lat > 40 & lat < 60 & long > 5 & long < 20)

# 绘制地图和数据点
plot2 <-ggplot() +  
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "white", color = "black") +  
  geom_point(data = data_counts, aes(x = LON, y = LAT, size = COUNT, color = COUNT), alpha = 0.7) +
  # geom_point(data = subset(data_counts, LAT > 40 & LAT < 60 & LON > 5 & LON < 20),
  #         aes(x = LON, y = LAT, size = COUNT, color = COUNT), alpha = 0.7) +
  scale_color_gradient(low = "pink", high = "darkred") +  
  scale_size_continuous(range = c(0.7, 0.7))+
  theme_minimal() +  
  labs(title = "PEP_ID Data Points on Europe Map", x = "Longitude", y = "Latitude")

# 保存图像到文件夹
ggsave("C:/Users/LENOVO/Desktop/lab/phylogeny/photo/plot1.jpg", plot = plot2, width = 6, height = 4, units = "in")






#* * 每个物种一个文件 *

# 指定保存文件的文件夹路径
output_folder <- "C:/Users/LENOVO/Desktop/lab/phylogeny/每个树种一个文件"

# 获取唯一的 treetype 值
unique_treetype <- unique(data$treetype)

# 根据每个 treetype 创建一个文件
for (id in unique_treetype) {
  # 选择当前 treetype 的数据
  subset_data <- data[data$treetype == id, ]
  
  # 构建文件名
  file_name <- paste0(id, "_BBCH_11.csv")
  
  # 保存数据到CSV文件
  write.csv(subset_data, file_name, row.names = FALSE)
  
  cat("File", file_name, "created.\n")
}





#* * 每个treetype 总共有多少个数据点*

# 统计每个 treetype 的数量
treetype_counts <- table(data$treetype)

# 创建一个数据框，包含 treetype 和对应的数量
result_df <- data.frame(treetype = names(treetype_counts), count = as.vector(treetype_counts))

# 打印数据框
cat("treetype\tCount\n")
cat("-----------------\n")
print(result_df, quote = FALSE, row.names = FALSE)
cat("\n")

# 将结果保存到CSV文件
write.csv(result_df, "treetype_counts.csv", row.names = FALSE)




#* * 每个treetype 总共有多少个数据点 其中具体展示每年有多少个数据点 *

# 设置工作目录，将路径更改为包含文件的目录
setwd("C:/Users/LENOVO/Desktop/lab/phylogeny")

# 读取文件
file_path <- "st BBCH 11 leaf.csv"  # 文件名
data <- read.csv(file_path, header = TRUE)  # 读取CSV文件，如果是其他格式，请适当修改

# 统计每个 treetype 在每年的数量
treetype_year_counts <- table(data$treetype, data$YEAR)

# 创建一个包含所有可能 treetype 和 YEAR 组合的数据框
all_combinations <- expand.grid(treetype = unique(data$treetype), YEAR = unique(data$YEAR), stringsAsFactors = FALSE)

# 查看 treetype_year_counts 表格的结构，确定唯一标识符列的名称
print(treetype_year_counts)

# 通过查看输出确定唯一标识符列的名称，假设为 "count"
unique_identifier <- "Freq"

# 合并数据，包括缺失组合，将缺失的计数设为0
result_df <- merge(all_combinations, as.data.frame(treetype_year_counts), by.x = c("treetype", "YEAR"), by.y = c("Var1", "Var2"), all.x = TRUE)

# 将缺失的计数设为0
result_df[[unique_identifier]][is.na(result_df[[unique_identifier]])] <- 0

# 打印数据框
cat("treetype\tYEAR\tCount\n")
cat("-----------------------\n")
print(result_df, quote = FALSE, row.names = FALSE)
cat("\n")

# 将结果保存到CSV文件
write.csv(result_df, "treetype_year_counts.csv", row.names = FALSE)

# 读取文件
file_path <- "treetype_year_counts.csv"  # 文件名
data <- read.csv(file_path, header = TRUE)  # 读取CSV文件，如果是其他格式，请适当修改

# 初步筛选 Freq 行
data_filtered0 <- data[data$Freq > 0, ]
data_filtered5 <- data[data$Freq > 5, ]
data_filtered10 <- data[data$Freq > 10, ]
data_filtered20 <- data[data$Freq > 20, ]

# 将过滤后的结果保存到新文件
write.csv(data_filtered0, "treetype_year_counts_filtered0.csv", row.names = FALSE)
write.csv(data_filtered5, "treetype_year_counts_filtered5.csv", row.names = FALSE)
write.csv(data_filtered10, "treetype_year_counts_filtered10.csv", row.names = FALSE)
write.csv(data_filtered20, "treetype_year_counts_filtered20.csv", row.names = FALSE)



# 读取文件
file_path <- "treetype_year_counts_filtered5.csv"  # 文件名
data <- read.csv(file_path, header = TRUE)  

# 统计不同的 treetype 类型
unique_treetypes <- unique(data$treetype)

# 输出不同 treetype 类型的数量
cat("Total number of unique treetypes:", length(unique_treetypes), "\n")

# 输出所有不同的 treetype 类型
cat("Unique treetypes:", paste(unique_treetypes, collapse = ", "), "\n")




# 读取文件
file_path <- "treetype_year_counts_filtered10.csv"  # 文件名
data <- read.csv(file_path, header = TRUE)  # 读取CSV文件，如果是其他格式，请适当修改

# 获取每个 treetype 对应的 YEAR 范围
treetype_ranges <- aggregate(YEAR ~ treetype, data, function(x) paste(range(x), collapse = "-"))

# 打印结果
print(treetype_ranges)

# 筛选符合条件的 treetype
filtered_treetypes <- subset(treetype_ranges, 
                             as.integer(substr(YEAR, 1, 4)) < 2000 & 
                               as.integer(substr(YEAR, 6, 9)) > 2014)

# 打印符合条件的 treetypes
print(filtered_treetypes)

# 将结果保存到新文件
write.csv(filtered_treetypes, "filtered_treetypes.csv", row.names = FALSE)




#* * 每个物种 趋势绘图 *

# 设置文件夹路径
folder_path <- "C:/Users/LENOVO/Desktop/lab/phylogeny/每个树种一个文件"

# 获取文件夹中的所有CSV文件
csv_files <- list.files(folder_path, pattern = ".csv", full.names = TRUE)

# 创建导出文件夹
output_folder <- "C:/Users/LENOVO/Desktop/lab/phylogeny/photojpg"
dir.create(output_folder, showWarnings = FALSE)

# 循环处理每个CSV文件
for (csv_file in csv_files) {
  # 从文件名中提取树种名称
  tree_species <- gsub(".csv", "", basename(csv_file))
  
  # 读取数据
  data <- read.csv(csv_file, header = TRUE)
  
  # 将YEAR和DAY转换为日期格式
  data$DATE <- as.Date(paste(data$YEAR, data$DAY, sep = "-"), format = "%Y-%j")
  
  # 提取YEAR和DAY列的唯一值
  years <- unique(data$YEAR)
  days <- unique(data$DAY)
  
  # 计算每年DAY的平均值和方差
  mean_day <- sapply(years, function(y) {
    subset_data <- data[data$YEAR == y, ]
    mean(subset_data$DAY)
  })
  var_day <- sapply(years, function(y) {
    subset_data <- data[data$YEAR == y, ]
    var(subset_data$DAY)
  })
  
  # 创建数据框存储年份、平均值和方差
  trend_data <- data.frame(YEAR = years, MEAN_DAY = mean_day, VAR_DAY = var_day)
  
  # 绘制趋势图
  p <- ggplot(trend_data, aes(x = YEAR, y = MEAN_DAY)) +
    geom_line() +
    geom_errorbar(aes(ymin = MEAN_DAY - sqrt(VAR_DAY), ymax = MEAN_DAY + sqrt(VAR_DAY)), width = 0.2) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = "Year", y = "Average DAY", title = paste("Trend of DAY for", tree_species)) +
    theme_minimal()
  
  # 导出图像
  output_file <- paste(output_folder, "/", tree_species, ".jpg", sep = "")
  ggsave(output_file, plot = p, width = 6, height = 4)
}
