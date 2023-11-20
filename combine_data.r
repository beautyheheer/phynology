#【把PEP CH 的文件 合并到 PEP  CH Combined 文件夹中】
# 设置源文件夹路径
source_folder <- "C:/Users/LENOVO/Desktop/lab/PEP CH"

# 设置目标文件夹路径
target_folder <- "C:/Users/LENOVO/Desktop/lab/PEP CH Combined"

# 获取所有文件夹的列表
subfolders <- list.dirs(source_folder, full.names = TRUE, recursive = FALSE)

# 定义目标文件名的正则表达式
pattern <- "PEP725_CH_(.*).csv"

# 遍历每个文件夹
for (subfolder in subfolders) {
  # 获取文件夹中的所有文件
  all_files <- list.files(subfolder, full.names = TRUE)
  
  # 筛选出包含目标文件的文件
  target_files <- grep(pattern, all_files, value = TRUE, ignore.case = TRUE)
  
  # 移动文件到目标文件夹
  file.copy(all_files, target_folder, overwrite = TRUE)
}






# 【把所有PEP的文件 合并到 PEP Combined 文件夹中】
# 设置源文件夹路径
source_folder <- "C:/Users/LENOVO/Desktop/lab/PEP"

# 设置目标文件夹路径
target_folder <- "C:/Users/LENOVO/Desktop/lab/PEP Combined"

# 获取所有文件夹的列表
subfolders <- list.dirs(source_folder, full.names = TRUE, recursive = FALSE)

# 定义目标文件名的正则表达式
pattern <- "PEP725_(.*).csv"

# 遍历每个文件夹
for (subfolder in subfolders) {
  # 获取文件夹中的所有文件
  all_files <- list.files(subfolder, full.names = TRUE)
  
  # 筛选出包含目标文件的文件
  target_files <- grep(pattern, all_files, value = TRUE, ignore.case = TRUE)
  
  # 移动文件到目标文件夹
  file.copy(all_files, target_folder, overwrite = TRUE)
}






# 【把PEP Combined 文件夹中 station 的文件 单独放到一个文件夹中】
# 设置源文件夹路径
source_folder <- "C:/Users/LENOVO/Desktop/lab/PEP Combined"

# 设置目标文件夹路径
target_folder <- "C:/Users/LENOVO/Desktop/lab/PEP Combined/stations"

# 创建目标文件夹（如果不存在）
if (!dir.exists(target_folder)) {
  dir.create(target_folder)
}

# 获取源文件夹中的所有文件
all_files <- list.files(source_folder, full.names = TRUE)

# 筛选出包含 "stations" 的文件
stations_files <- grep("stations", all_files, value = TRUE, ignore.case = TRUE)

# 移动文件到目标文件夹
file.rename(stations_files, file.path(target_folder, basename(stations_files)))






# 【把PEP Combined 文件夹中 每一个文件对应BBCH的数据放到一个文件夹中 比如 BBCH 11 leaf】
# 安装和加载必要的包（如果未安装的话）
install.packages("readxl")
library(readxl)
install.packages("openxlsx")
library(openxlsx)

# 设置文件夹路径
folder_path <- "C:/Users/LENOVO/Desktop/lab/PEP Combined"

# 获取文件夹中的所有 CSV 文件
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# 初始化一个空的数据框
combined_data <- data.frame()

# 循环处理每个 CSV 文件
for (file in files) {
  # 读取原始 CSV 文件
  original_data <- read.csv(file, sep = ";", header = TRUE)
  
  # 获取文件名的三个部分
  file_name <- basename(file)
  file_parts <- unlist(strsplit(gsub("\\.csv", "", file_name), "_"))
  
  # 第一部分为固定字符串 "PEP725"
  first_part <- "PEP725"
  
  # 第三部分为第二部分之后的内容
  third_part <- gsub(paste0(first_part, "_", file_parts[2], "_"), "", file_name)
  third_part <- gsub("\\.csv$", "", third_part)
  
  # 提取 'AT' 部分作为 'Country'
  country_part <- file_parts[2]
  
  # 创建新的数据框
  new_data <- data.frame(
    Country = country_part,  # 第一部分内容
    treetype = third_part,  # 第二部分内容
    original_data = original_data  # 原始数据的列
  )
  
  # 仅选择 BBCH 列中值为 11 13 60 64 65 的数据
  filtered_data <- subset(new_data, original_data[, 2] == 60)
  
  # 合并到总的数据框
  combined_data <- rbind(combined_data, filtered_data)
}

# 设置新 csv 文件路径
new_csv_path <- "C:/Users/LENOVO/Desktop/lab/PEP combinecode/BBCH 60 flower.csv"

# 将合并后的数据写入 csv 文件
write.csv(combined_data, new_csv_path, row.names = FALSE )

# 设置 CSV 文件路径
csv_path <- "C:/Users/LENOVO/Desktop/lab/PEP combinecode/BBCH 60 flower.csv"

# 读取 CSV 文件
data <- read.csv(csv_path,header=TRUE)






# 【 看一下总共有几行数据 】
# 读取文件到数据框
data <- read.csv("your_file.csv")

# 查看数据框中的行数（即文件中的数据行数）
total_rows <- nrow(data)
print(total_rows)  # 打印文件中的总行数






#【把 所有的PEP station 放到一起】
# 设置文件夹路径
folder_path <- "C:/Users/LENOVO/Desktop/lab/PEP stations"

# 获取文件夹中的所有 CSV 文件
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# 初始化一个空的数据框来存储所有数据
all_data <- data.frame()

# 循环处理每个 CSV 文件并合并数据
for (file in files) {
  # 读取每个 CSV 文件
  current_data <- read.csv(file, sep = ";")
  
  # 提取文件名中的中间部分作为 'Country' 列的内容
  country_name <- gsub(".*PEP725_(.*)_stations\\.csv", "\\1", file)
  current_data$Country <- country_name
  
  # 将数据合并到 all_data 数据框中
  all_data <- rbind(all_data, current_data)
}

# 重新排列列的顺序，将 'Country' 列放在第一列
all_data <- all_data[, c("Country", "PEP_ID", "National_ID", "LON", "LAT", "ALT")]

# 将合并后的数据写入一个新的 CSV 文件
write.csv(all_data, file = "C:/Users/LENOVO/Desktop/lab/PEP stations/allstations.csv", row.names = FALSE)






#【计算文件夹中的总行数】
# 设置文件夹路径
folder_path <- "C:/Users/LENOVO/Desktop/lab/PEP stations"

# 获取文件夹中的所有 CSV 文件
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# 计算所有文件总共的行数
total_lines <- sum(sapply(files, function(file) {
  count_lines <- length(readLines(file))
  return(count_lines)
}))

# 打印总行数
print(total_lines)







#【合并两个csv文件 把站点和物候数据放一起】
# 导入所需的包
library(dplyr)

# 读取两个 CSV 文件
path_bbch <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/BBCH 65 flower 1.csv"
path_allstations <- "C:/Users/LENOVO/Desktop/lab/allstations.csv"

bbch_data <- read.csv(file = path_bbch, header = TRUE)
allstations_data <- read.csv(file = path_allstations, header = TRUE)

# 使用 dplyr 包中的 merge 函数，基于 'PEP_ID' 合并两个数据框
merged_data <- merge(bbch_data, allstations_data, by = "PEP_ID", all.x = TRUE)

# 选择要保留的列，并将其中一个 'Country' 列重命名为 'Country'
merged_data <- merged_data %>%
  select(-matches("\\.y")) %>%
  rename(Country = Country.x)

# 定义新文件的保存路径
output_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/st BBCH 65 flower 1.csv"

# 将合并后的数据保存为新的 CSV 文件
write.csv(merged_data, file = output_path, row.names = FALSE)





# 【文件夹中的信息统计】
# 读取 CSV 文件
file_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/st BBCH 11 leaf.csv"
data <- read.csv(file_path, header = TRUE)

# 选择 Country 列为 "DE" 的子集
subset_DE <- subset(data, Country == "DE")
subset_AT <- subset(data, Country == "AT")

# 统计 PEP_ID 列的类别数量
pep_id_count <- table(data$PEP_ID)

# 统计 Country 列的类别数量 对结果进行排序（按照数量从大到小）
country_count <- table(data$Country)
sorted_country_count <- sort(country_count, decreasing = TRUE)

# 统计 treetype 列的类别数量
treetype_count <- table(data$treetype)
sorted_treetype_count <- sort(treetype_count, decreasing = TRUE)

# 对子集中 treetype 列的类别进行数量统计
treetype_count_DE <- table(subset_DE$treetype)
sorted_treetype_count_DE <- sort(treetype_count_DE, decreasing = TRUE)

treetype_count_AT <- table(subset_AT$treetype)
sorted_treetype_count_AT <- sort(treetype_count_AT, decreasing = TRUE)

# 输出每个类别的数量统计结果
print("PEP_ID Count:")
print(pep_id_count)

# 输出排序后的结果
print("Country Count (Sorted):")
print(sorted_country_count)

# 输出排序后的结果
print("treetype Count (Sorted):")
print(sorted_treetype_count)

# 输出 Country 列为 "DE" 中 treetype 列的数量统计结果
print("treetype Count in Country DE:")
print(sorted_treetype_count_DE)

# 输出 Country 列为 "DE" 中 treetype 列的数量统计结果
print("treetype Count in Country AT:")
print(sorted_treetype_count_AT)








# 【取文件夹中的子集】
# 读取 CSV 文件
file_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/st BBCH 11 leaf.csv"
data <- read.csv(file_path, header = TRUE)

# 选择 Country 列为 "DE" 的子集
subset_DE <- subset(data, Country == "DE")

# 确定新文件的保存路径
new_file_path <- "C:/Users/LENOVO/Desktop/lab/Filtered_data_code_PEP/DE_11.csv"

# 将符合条件的子集保存到新的 CSV 文件中
write.csv(subset_DE, file = new_file_path, row.names = FALSE)

