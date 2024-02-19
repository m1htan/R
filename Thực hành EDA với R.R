# Đọc file dữ liệu và thêm thư viện
library(tidyverse)
library(readxl)

data <- read_excel("C:/Users/Minh Tan/Documents/Bai tap R/Price-Data-W2.xlsx")

# Kiểm tra đặc trưng của biến:
dim(data)
nrow(data)
ncol(data)
str(data)
basic_info <- str(data)
statistical_description <- summary(data)

#Vẽ biểu đồ
ggplot(data = data) + geom_bar(mapping = aes(x = ACB.HM.Close))

