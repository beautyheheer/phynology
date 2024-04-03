# 设置工作目录
setwd("C:/Users/LENOVO/Desktop/lab/0 processed data/all csv data_PEP")

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
  treetype <- gsub("PEP725_", "", file_parts[3])
  
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
