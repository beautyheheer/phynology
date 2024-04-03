# ---- 合并为一个文件 ----
# 设置工作目录
setwd("C:/Users/LENOVO/Desktop/lab")

# 列出所有的CSV文件
file_list <- list.files(pattern = "*.csv")

# 初始化一个空的数据框，用于后续合并所有文件
all_data <- data.frame()

# 循环处理每个文件
for (file_name in file_list) {
  # 读取CSV文件
  data <- read.csv(file_name, sep = ";", header = TRUE)
  
  # 从文件名提取国家代码和树种名称
  file_parts <- strsplit(file_name, "_")[[1]]
  country_code <- file_parts[2]
  
  # 从第三部分开始的所有部分合并起来作为树种名称
  treetype <- paste(file_parts[-(1:2)], collapse = "_")
  
  # 移除文件扩展名（如果存在）
  treetype <- gsub("\\.csv$", "", treetype)
  
  # 添加国家代码和树种名称列
  data$country <- country_code
  data$treetype <- treetype
  
  # 合并到总的数据框中
  all_data <- rbind(all_data, data)
}

# 重命名列名称
colnames(all_data) <- c("PEP_ID", "BBCH", "YEAR", "DAY", "country", "treetype")

# 写入到新的CSV文件
write.csv(all_data, "data_original_all.csv", row.names = FALSE)

print(unique(all_data$treetype))



# ---- 拆分数据库 ----

# 根据BBCH列的值拆分数据框
bbch_groups <- split(all_data, all_data$BBCH)

# 指定您想要保存CSV文件的文件夹路径
output_folder <- "C:/Users/LENOVO/Desktop/lab"

# 遍历列表，将每个BBCH值的数据集写入到单独的CSV文件中
for(bbch in names(bbch_groups)) {
  # 创建文件名，例如：BBCH_11.csv
  # 确保文件路径和文件名之间有一个斜杠
  file_name <- paste0(output_folder, "/BBCH_", bbch, ".csv")
  
  # 将数据写入文件，不包括行名
  write.csv(bbch_groups[[bbch]], file_name, row.names = FALSE)
}
