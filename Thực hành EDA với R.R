# Đọc file dữ liệu và thêm thư viện
library(tidyverse)
library(dplyr)
library(lubridate)

library(readxl)
library(writexl)

data <- read_excel("C:/Users/Minh Tan/Documents/Bai tap R/Price-Data-W2.xlsx")

write_xlsx(data, "data.xlsx")

# Kiểm tra đặc trưng của biến:
dim(data)
nrow(data)
ncol(data)
str(data)
basic_info <- str(data)
statistical_description <- summary(data)

# Tiền xử lý dữ liệu
null_counts <- colSums(is.na(data)) # Kiểm tra các giá trị null 
null_counts

data <- na.omit(data) # Xóa các giá trị null 
null_counts <- colSums(is.na(data))  
null_counts

# Tách dữ liệu thời gian từ cột Timestamp 
data <- data %>%
mutate(Timestamp = ymd(Timestamp),  
        Weekday = wday(Timestamp),   
        Day = day(Timestamp),         
        Month = month(Timestamp),     
        Year = year(Timestamp))

# Vẽ biểu đồ Histogram
ggplot(data=data) + geom_histogram(mapping = aes(x = ACB.HM.Close), binwidth = 200)
ggplot(data=data) + geom_histogram(mapping = aes(x = ACB.HM.Volume), binwidth = 200000)
ggplot(data=data) + geom_histogram(mapping = aes(x = BAB.HN.Close), binwidth = 200)
ggplot(data=data) + geom_histogram(mapping = aes(x = BAB.HN.Volume), binwidth = 20000)

# So sánh từng cặp biến 
ggplot(data = data, mapping = aes(x = ACB.HM.Close, colour = cut)) + geom_freqpoly(binwidth = 0.1)




















