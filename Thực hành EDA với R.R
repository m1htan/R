# Đọc file dữ liệu và thêm thư viện
library(tidyverse)
library(dplyr)
library(lubridate)

library(readxl)
library(writexl)

data <- read_excel("C:/Users/Minh Tan/Documents/Bai tap R/Price-Data-W2.xlsx")

write_xlsx(data, "data.xlsx")

#################################################################################################################

# Tách dữ liệu cho ACB.HM vào df1
df1 <- data[, c("ID", "Timestamp", "ACB.HM.Close", "ACB.HM.Volume")]

# Tách dữ liệu cho BAB.HN vào df2
df2 <- data[, c("ID", "Timestamp", "BAB.HN.Close", "BAB.HN.Volume")]

df1 <- na.exclude(df1)
df2 <- na.exclude(df2)

df1
df2
length(which(is.na(df1$ACB.HM.Close)))
length(which(is.na(df1$ACB.HM.Volume)))
length(which(is.na(df2$BAB.HN.Close)))
length(which(is.na(df2$BAB.HN.Volume)))


data <- merge(df1, df2, by = c("ID", "Timestamp"), all = TRUE)

#################################################################################################################

# Kiểm tra đặc trưng của biến:
dim(data)
nrow(data)
ncol(data)
str(data)
basic_info <- str(data)
statistical_description <- summary(data)

#################################################################################################################

# Tách dữ liệu thời gian từ cột Timestamp 
data <- data %>%
mutate(Timestamp = ymd(Timestamp),  
        Weekday = wday(Timestamp),   
        Day = day(Timestamp),         
        Month = month(Timestamp),     
        Year = year(Timestamp))

# Thêm hai biến giá trị giao dịch = giá đóng cửa * volume
data$TransactionValueACB = data$ACB.HM.Volume*data$ACB.HM.Close
data$TransactionValueBAB = data$BAB.HN.Volume*data$BAB.HN.Close

data <- data %>% 
mutate(TransactionValueACB = ACB.HM.Volume * ACB.HM.Close,
      TransactionValueBAB = BAB.HN.Volume * BAB.HN.Close)

#################################################################################################################

# Vẽ biểu đồ Histogram
ggplot(data=data, main = ACB.HM.Close) + 
  geom_histogram(mapping = aes(x = ACB.HM.Close), binwidth = 200)

ggplot(data=data, main = ACB.HM.Volume) + 
  geom_histogram(mapping = aes(x = ACB.HM.Volume), binwidth = 200000)

ggplot(data=data, main = BAB.HN.Close) + 
  geom_histogram(mapping = aes(x = BAB.HN.Close), binwidth = 200)

ggplot(data=data, main = BAB.HN.Volume) + 
  geom_histogram(mapping = aes(x = BAB.HN.Volume), binwidth = 20000)

# So sánh từng cặp biến 
ggplot(data=data, mapping = aes(x = ACB.HM.Close, y = BAB.HN.Close)) + 
  geom_point() +
  ggtitle('So sánh giá đóng cửa giữa 2 biến ACB.HM và BAB.HN')

ggplot(data=data, mapping = aes(x = ACB.HM.Volume, y = BAB.HN.Volume)) + 
  geom_point() +
  ggtitle('So sánh khối lượng giao dịch giữa 2 biến ACB.HM và BAB.HN')

#################################################################################################################

# Biểu đồ boxplot cho biến ACB.HM.Close
ggplot(data, aes(y = ACB.HM.Close)) +
  geom_boxplot() +
  labs(title = "Biểu Đồ Boxplot cho ACB.HM.Close",
       y = "ACB.HM.Close") +
  theme_minimal()

# Biểu đồ boxplot cho biến ACB.HM.Volume
ggplot(data, aes(y = ACB.HM.Volume)) +
  geom_boxplot() +
  labs(title = "Biểu Đồ Boxplot cho ACB.HM.Volume",
       y = "ACB.HM.Volume") +
  theme_minimal()

# Biểu đồ boxplot cho biến BAB.HN.Close
ggplot(data, aes(y = BAB.HN.Close)) +
  geom_boxplot() +
  labs(title = "Biểu Đồ Boxplot cho BAB.HN.Close",
       y = "BAB.HN.Close") +
  theme_minimal()

# Biểu đồ boxplot cho biến BAB.HN.Volume
ggplot(data, aes(y = BAB.HN.Volume)) +
  geom_boxplot() +
  labs(title = "Biểu Đồ Boxplot cho BAB.HN.Volume",
       y = "BAB.HN.Volume") +
  theme_minimal()

#################################################################################################################

#Xóa outliner khỏi tập dữ liệu
# Biểu đồ boxplot cho biến ACB.HM.Close (đã loại bỏ outlier)
ggplot(data, aes(y = ACB.HM.Close)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Biểu Đồ Boxplot cho ACB.HM.Close",
       y = "ACB.HM.Close") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#E0E0E0"),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = quantile(data$ACB.HM.Close, c(0.05, 0.95))) 

# Biểu đồ boxplot cho biến ACB.HM.Volume (đã loại bỏ outlier)
ggplot(data, aes(y = ACB.HM.Volume)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Biểu Đồ Boxplot cho ACB.HM.Volume",
       y = "ACB.HM.Volume") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#E0E0E0"),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = quantile(data$ACB.HM.Volume, c(0.05, 0.95)))

# Biểu đồ boxplot cho biến BAB.HN.Close (đã loại bỏ outlier)
ggplot(data, aes(y = BAB.HN.Close)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Biểu Đồ Boxplot cho BAB.HN.Close",
       y = "BAB.HN.Close") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#E0E0E0"),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = quantile(data$BAB.HN.Close, c(0.05, 0.95)))  

# Biểu đồ boxplot cho biến BAB.HN.Volume (đã loại bỏ outlier)
ggplot(data, aes(y = BAB.HN.Volume)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Biểu Đồ Boxplot cho BAB.HN.Volume",
       y = "BAB.HN.Volume") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#E0E0E0"),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = quantile(data$BAB.HN.Volume, c(0.05, 0.95)))

#################################################################################################################

# Tóm tắt dữ liệu để chọn giá đóng cửa cao nhất của mỗi năm của ngân hàng ACB
yearly_max_close <- data %>%
  group_by(Year) %>%
  summarize(Max_Close = max(ACB.HM.Close))
  
# Tạo biểu đồ đường cho giá đóng cửa cao nhất của mỗi năm của ngân hàng ACB 
ggplot(yearly_max_close, aes(x = Year, y = Max_Close, group=1)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Đóng Cửa Cao Nhất của ACB theo Năm",
       x = "Năm",
       y = "Giá Đóng Cửa Cao Nhất") +
  theme_minimal()


# Tóm tắt dữ liệu để chọn giá đóng cửa cao nhất của mỗi năm cho ngân hàng BAB.HN
yearly_max_close_bab <- data %>%
  group_by(Year) %>%
  summarize(Max_Close = max(BAB.HN.Close))

# Tạo biểu đồ đường cho giá đóng cửa cao nhất của mỗi năm cho ngân hàng BAB.HN
ggplot(yearly_max_close_bab, aes(x = Year, y = Max_Close)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Đóng Cửa Cao Nhất của BAB.HN theo Năm",
       x = "Năm",
       y = "Giá Đóng Cửa Cao Nhất") +
  theme_minimal()

#################################################################################################################

# Tóm tắt dữ liệu để chọn giá đóng cửa cao nhất của mỗi năm cho ngân hàng ACB.HM và BAB.HN
yearly_max_close <- data %>%
  group_by(Year) %>%
  summarize(Max_Close_ACB = max(ACB.HM.Close),
            Max_Close_BAB = max(BAB.HN.Close))

# Tạo biểu đồ đường cho giá đóng cửa cao nhất của mỗi năm cho cả ACB.HM và BAB.HN
ggplot(yearly_max_close, aes(x = Year, group=1)) +
  geom_line(aes(y = Max_Close_ACB, color = "ACB.HM")) +
  geom_point(aes(y = Max_Close_ACB, color = "ACB.HM")) +
  geom_line(aes(y = Max_Close_BAB, color = "BAB.HN")) +
  geom_point(aes(y = Max_Close_BAB, color = "BAB.HN")) +
  labs(title = "Giá Đóng Cửa Cao Nhất của ACB.HM và BAB.HN theo Năm",
       x = "Năm",
       y = "Giá Đóng Cửa Cao Nhất",
       color = "Ngân hàng") +
  scale_color_manual(values = c("ACB.HM" = "blue", "BAB.HN" = "red")) +
  theme_minimal()

#################################################################################################################

# Chuyển đổi cột thời gian sang định dạng thời gian
data$Timestamp <- as.POSIXct(data$Timestamp)

# Trích xuất thông tin về năm và tháng từ cột Timestamp
data <- data %>%
  mutate(Year = year(Timestamp),
         Month = month(Timestamp))
        
# Lọc dữ liệu cho năm 2021
data_2021 <- data %>%
  filter(Year == 2021)

# Tóm tắt dữ liệu để chọn giá đóng cửa cao nhất của mỗi tháng
monthly_max_close <- data_2021 %>%
  group_by(Month) %>%
  summarize(Max_Close = max(ACB.HM.Close))

# Tạo biểu đồ đường cho giá đóng cửa cao nhất của mỗi tháng trong năm 2021
ggplot(monthly_max_close, aes(x = Month, y = Max_Close)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Đóng Cửa Cao Nhất của ACB theo Tháng trong Năm 2021",
       x = "Tháng",
       y = "Giá Đóng Cửa Cao Nhất") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + 
  theme_minimal()


# Lọc dữ liệu cho năm 2021
data_2021 <- data %>%
  filter(Year == 2021)

# Tóm tắt dữ liệu để chọn giá đóng cửa cao nhất của mỗi tháng
monthly_max_close_bab <- data_2021 %>%
  group_by(Month) %>%
  summarize(Max_Close_BAB = max(BAB.HN.Close))

# Tạo biểu đồ đường cho giá đóng cửa cao nhất của cổ phiếu BAB theo tháng trong năm 2021
ggplot(monthly_max_close_bab, aes(x = Month, y = Max_Close_BAB)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Đóng Cửa Cao Nhất của BAB theo Tháng trong Năm 2021",
       x = "Tháng",
       y = "Giá Đóng Cửa Cao Nhất") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + 
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho tháng bảy năm 2021
data_july_2021 <- data %>%
  filter(Year == 2021, Month == 7)

# Tạo biểu đồ đường cho giá đóng cửa cao nhất của cổ phiếu ACB trong tháng bảy năm 2021
ggplot(data_july_2021, aes(x = Timestamp, y = ACB.HM.Close)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Đóng Cửa Cao Nhất của ACB trong Tháng 7 Năm 2021",
       x = "Ngày",
       y = "Giá Đóng Cửa Cao Nhất") +
  theme_minimal()


# Lọc dữ liệu cho tháng ba năm 2021
data_march_2021 <- data %>%
  filter(Year == 2021, Month == 3)

# Tạo biểu đồ đường cho giá đóng cửa cao nhất của cổ phiếu BAB trong tháng ba năm 2021
ggplot(data_march_2021, aes(x = Timestamp, y = BAB.HN.Close)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Đóng Cửa Cao Nhất của BAB trong Tháng 3 Năm 2021",
       x = "Ngày",
       y = "Giá Đóng Cửa Cao Nhất") +
  theme_minimal()

#################################################################################################################

# Tính toán khối lượng giao dịch cao nhất của mỗi năm cho ACB.HM và BAB.HN
yearly_max_volume <- data %>%
  group_by(Year = year(Timestamp)) %>%
  summarize(Max_Volume_ACB = max(ACB.HM.Volume, na.rm = TRUE),
            Max_Volume_BAB = max(BAB.HN.Volume, na.rm = TRUE))

# Vẽ biểu đồ đường cho khối lượng giao dịch cao nhất của mỗi năm
ggplot(yearly_max_volume, aes(x = Year)) +
  geom_line(aes(y = Max_Volume_ACB, color = "ACB.HM")) +
  geom_point(aes(y = Max_Volume_ACB, color = "ACB.HM")) +
  geom_line(aes(y = Max_Volume_BAB, color = "BAB.HN")) +
  geom_point(aes(y = Max_Volume_BAB, color = "BAB.HN")) +
  labs(title = "Khối Lượng Giao Dịch Cao Nhất của ACB.HM và BAB.HN theo Năm",
       x = "Năm",
       y = "Khối Lượng Giao Dịch Cao Nhất",
       color = "Ngân hàng") +
  scale_color_manual(values = c("ACB.HM" = "blue", "BAB.HN" = "red")) +
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho năm 2021
data_2021 <- data %>%
  filter(year(Timestamp) == 2021)

# Tính toán khối lượng giao dịch cao nhất của ACB cho mỗi tháng trong năm 2021
monthly_max_volume_acb_2021 <- data_2021 %>%
  group_by(Month = month(Timestamp)) %>%
  summarize(Max_Volume_ACB = max(ACB.HM.Volume, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện khối lượng giao dịch cao nhất theo từng tháng
ggplot(monthly_max_volume_acb_2021, aes(x = Month, y = Max_Volume_ACB)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Khối Lượng Giao Dịch Cao Nhất Của ACB Theo Tháng Trong Năm 2021",
       x = "Tháng",
       y = "Khối Lượng Giao Dịch Cao Nhất") +
  theme_minimal()


# Lọc dữ liệu cho năm 2021
data_2021_bab <- data %>%
  filter(year(Timestamp) == 2021)

# Tính toán khối lượng giao dịch cao nhất của BAB cho mỗi tháng trong năm 2021
monthly_max_volume_bab_2021 <- data_2021_bab %>%
  group_by(Month = month(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Max_Volume_BAB = max(BAB.HN.Volume, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện khối lượng giao dịch cao nhất theo từng tháng
ggplot(monthly_max_volume_bab_2021, aes(x = Month, y = Max_Volume_BAB, group=1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) + 
  labs(title = "Khối Lượng Giao Dịch Cao Nhất Của BAB Theo Tháng Trong Năm 2021",
       x = "Tháng",
       y = "Khối Lượng Giao Dịch Cao Nhất") +
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho tháng 2 năm 2021
data_feb_2021_acb <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 2)

# Tính toán khối lượng giao dịch cao nhất mỗi ngày trong tháng 2 năm 2021 cho ACB
daily_max_volume_acb_feb_2021 <- data_feb_2021_acb %>%
  group_by(Date = as.Date(Timestamp)) %>%
  summarize(Max_Volume_ACB = max(ACB.HM.Volume, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện khối lượng giao dịch cao nhất của ACB trong tháng 2 năm 2021
ggplot(daily_max_volume_acb_feb_2021, aes(x = Date, y = Max_Volume_ACB)) +
  geom_line() +
  geom_point() +
  labs(title = "Khối Lượng Giao Dịch Cao Nhất của ACB trong Tháng 2 Năm 2021",
       x = "Ngày",
       y = "Khối Lượng Giao Dịch Cao Nhất") +
  theme_minimal()


# Lọc dữ liệu cho tháng 5 năm 2021 của ngân hàng BAB
data_may_2021_bab <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 5)

# Tính toán khối lượng giao dịch cao nhất mỗi ngày trong tháng 5 năm 2021 cho BAB
daily_max_volume_bab_may_2021 <- data_may_2021_bab %>%
  group_by(Date = as.Date(Timestamp)) %>%
  summarize(Max_Volume_BAB = max(BAB.HN.Volume, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện khối lượng giao dịch cao nhất của BAB trong tháng 5 năm 2021
ggplot(daily_max_volume_bab_may_2021, aes(x = Date, y = Max_Volume_BAB)) +
  geom_line() +
  geom_point() +
  labs(title = "Khối Lượng Giao Dịch Cao Nhất của BAB trong Tháng 5 Năm 2021",
       x = "Ngày",
       y = "Khối Lượng Giao Dịch Cao Nhất") +
  theme_minimal()

#################################################################################################################

# Tóm tắt dữ liệu để chọn giá trị giao dịch cao nhất của mỗi năm cho ACB và BAB
yearly_max_transaction <- data %>%
  group_by(Year) %>%
  summarize(Max_TransactionValueACB = max(TransactionValueACB),
            Max_TransactionValueBAB = max(TransactionValueBAB))

# Gộp dữ liệu thành một dataframe
yearly_max_transaction <- gather(yearly_max_transaction, Bank, Max_Value, -Year)

# Vẽ biểu đồ line
ggplot(yearly_max_transaction, aes(x = Year, y = Max_Value, color = Bank)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá trị giao dịch cao nhất qua từng năm của ACB và BAB",
       x = "Năm",
       y = "Giá trị giao dịch cao nhất",
       color = "Ngân hàng") +
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho năm 2021
data_2021_acb <- data %>%
  filter(year(Timestamp) == 2021)

# Tính toán giá trị giao dịch cao nhất của ACB cho mỗi tháng trong năm 2021
monthly_max_transaction_acb_2021 <- data_2021_acb %>%
  group_by(Month = month(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Max_TransactionValueACB = max(TransactionValueACB, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện giá trị giao dịch cao nhất của ACB theo từng tháng trong năm 2021
ggplot(monthly_max_transaction_acb_2021, aes(x = Month, y = Max_TransactionValueACB, group=1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) + 
  labs(title = "Giá trị Giao Dịch Cao Nhất của ACB Theo Tháng Trong Năm 2021",
       x = "Tháng",
       y = "Giá trị Giao Dịch Cao Nhất") +
  scale_x_discrete(labels = month.abb) + 
  theme_minimal()


# Lọc dữ liệu cho năm 2021
data_2021_bab <- data %>%
  filter(year(Timestamp) == 2021)

# Tính toán giá trị giao dịch cao nhất của BAB cho mỗi tháng trong năm 2021
monthly_max_transaction_bab_2021 <- data_2021_bab %>%
  group_by(Month = month(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Max_TransactionValueBAB = max(TransactionValueBAB, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện giá trị giao dịch cao nhất của BAB theo từng tháng trong năm 2021
ggplot(monthly_max_transaction_bab_2021, aes(x = Month, y = Max_TransactionValueBAB, group=1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) + 
  labs(title = "Giá trị Giao Dịch Cao Nhất của BAB Theo Tháng Trong Năm 2021",
       x = "Tháng",
       y = "Giá trị Giao Dịch Cao Nhất") +
  scale_x_discrete(labels = month.abb) + 
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho tháng 7 năm 2021 của ngân hàng ACB
data_july_2021_acb <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 7)

# Tính toán giá trị giao dịch cao nhất mỗi ngày trong tháng 7 năm 2021 cho ACB
daily_max_transaction_acb_july_2021 <- data_july_2021_acb %>%
  group_by(Date = as.Date(Timestamp)) %>%
  summarize(Max_TransactionValueACB = max(TransactionValueACB, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện giá trị giao dịch cao nhất của ACB trong tháng 7 năm 2021
ggplot(daily_max_transaction_acb_july_2021, aes(x = Date, y = Max_TransactionValueACB)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Trị Giao Dịch Cao Nhất của ACB trong Tháng 7 Năm 2021",
       x = "Ngày",
       y = "Giá Trị Giao Dịch Cao Nhất") +
  theme_minimal()


# Lọc dữ liệu cho tháng 5 năm 2021 của ngân hàng BAB
data_may_2021_bab <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 5)

# Tính toán giá trị giao dịch cao nhất mỗi ngày trong tháng 5 năm 2021 cho BAB
daily_max_transaction_bab_may_2021 <- data_may_2021_bab %>%
  group_by(Date = as.Date(Timestamp)) %>%
  summarize(Max_TransactionValueBAB = max(TransactionValueBAB, na.rm = TRUE))

# Vẽ biểu đồ đường thể hiện giá trị giao dịch cao nhất của BAB trong tháng 5 năm 2021
ggplot(daily_max_transaction_bab_may_2021, aes(x = Date, y = Max_TransactionValueBAB)) +
  geom_line() +
  geom_point() +
  labs(title = "Giá Trị Giao Dịch Cao Nhất của BAB trong Tháng 5 Năm 2021",
       x = "Ngày",
       y = "Giá Trị Giao Dịch Cao Nhất") +
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho tháng 7 của ngân hàng ACB
data_july_acb <- data %>%
  filter(Year == 2021, Month == 7) %>%
  select(Timestamp, ACB.HM.Close) %>%
  mutate(Weekday = wday(Timestamp))

# Tính toán giá đóng cửa trung bình theo thứ trong tuần
weekday_avg_close <- data_july_acb %>%
  group_by(Weekday) %>%
  summarize(Avg_Close = mean(ACB.HM.Close))

# Đổi tên thứ trong tuần từ số sang tên
weekday_avg_close$Weekday <- factor(weekday_avg_close$Weekday, levels = 1:7, labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

# Vẽ biểu đồ cột thể hiện giá đóng cửa trung bình theo thứ trong tuần
ggplot(weekday_avg_close, aes(x = Weekday, y = Avg_Close)) +
  geom_col(fill = "skyblue", color = "black", width = 0.5) +
  labs(title = "Giá Đóng Cửa Trung Bình Theo Thứ Trong Tháng 7 Của Ngân Hàng ACB",
       x = "Thứ trong Tuần",
       y = "Giá Đóng Cửa Trung Bình") +
  theme_minimal()


# Lọc dữ liệu cho tháng 3 của ngân hàng BAB
data_march_bab <- data %>%
  filter(Year == 2021, Month == 3)

# Tính giá đóng cửa trung bình theo thứ trong tháng 3 của ngân hàng BAB
weekday_avg_close_bab_march <- data_march_bab %>%
  group_by(Weekday = wday(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Avg_Close = mean(BAB.HN.Close, na.rm = TRUE))

# Vẽ biểu đồ cột với cột nhỏ lại
ggplot(weekday_avg_close_bab_march, aes(x = Weekday, y = Avg_Close)) +
  geom_col(fill = "skyblue", color = "black", width = 0.5) +
  labs(title = "Giá Đóng Cửa Trung Bình Theo Thứ Trong Tháng 3 Của Ngân Hàng BAB",
       x = "Thứ trong Tuần",
       y = "Giá Đóng Cửa Trung Bình") +
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho tháng 2 của ngân hàng ACB
data_feb_acb <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 2)

# Tính khối lượng giao dịch trung bình theo thứ trong tháng 2 của ngân hàng ACB
weekday_avg_volume_acb_feb <- data_feb_acb %>%
  group_by(Weekday = wday(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Avg_Volume = mean(ACB.HM.Volume, na.rm = TRUE))

# Vẽ biểu đồ cột
ggplot(weekday_avg_volume_acb_feb, aes(x = Weekday, y = Avg_Volume)) +
  geom_col(fill = "lightblue", color = "black", width=0.5) +
  labs(title = "Khối Lượng Giao Dịch Trung Bình Theo Thứ Trong Tháng 2 của ngân hàng ACB",
       x = "Thứ trong Tuần",
       y = "Khối Lượng Giao Dịch Trung Bình") +
  theme_minimal()

# Lọc dữ liệu cho tháng 5 của ngân hàng BAB
data_may_bab <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 5)

# Tính khối lượng giao dịch trung bình theo thứ trong tháng 5 của ngân hàng BAB
weekday_avg_volume_bab_may <- data_may_bab %>%
  group_by(Weekday = wday(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Avg_Volume = mean(BAB.HN.Volume, na.rm = TRUE))

# Vẽ biểu đồ cột
ggplot(weekday_avg_volume_bab_may, aes(x = Weekday, y = Avg_Volume)) +
  geom_col(fill = "lightblue", color = "black", width=0.5) +
  labs(title = "Khối Lượng Giao Dịch Trung Bình Theo Thứ Trong Tháng 5 của ngân hàng BAB",
       x = "Thứ trong Tuần",
       y = "Khối Lượng Giao Dịch Trung Bình") +
  theme_minimal()

#################################################################################################################

# Lọc dữ liệu cho tháng 7 của ngân hàng ACB
data_july_acb <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 7)

# Tính giá trị giao dịch trung bình theo thứ trong tháng 7 của ngân hàng ACB
weekday_avg_transaction_acb_july <- data_july_acb %>%
  group_by(Weekday = wday(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Avg_TransactionValueACB = mean(TransactionValueACB, na.rm = TRUE))

# Vẽ biểu đồ cột
ggplot(weekday_avg_transaction_acb_july, aes(x = Weekday, y = Avg_TransactionValueACB)) +
  geom_col(fill = "lightblue", color = "black", width=0.5) +
  labs(title = "Giá Trị Giao Dịch Trung Bình Theo Thứ Trong Tháng 7 của ngân hàng ACB",
       x = "Thứ trong Tuần",
       y = "Giá Trị Giao Dịch Trung Bình (ACB)",
       fill = "Ngân hàng") +
  theme_minimal()

# Lọc dữ liệu cho tháng 5 của ngân hàng BAB
data_may_bab <- data %>%
  mutate(Year = year(Timestamp), Month = month(Timestamp)) %>%
  filter(Year == 2021, Month == 5)

# Tính giá trị giao dịch trung bình theo thứ trong tháng 5 của ngân hàng BAB
weekday_avg_transaction_bab_may <- data_may_bab %>%
  group_by(Weekday = wday(Timestamp, label = TRUE, abbr = TRUE)) %>%
  summarize(Avg_TransactionValueBAB = mean(TransactionValueBAB, na.rm = TRUE))

# Vẽ biểu đồ cột
ggplot(weekday_avg_transaction_bab_may, aes(x = Weekday, y = Avg_TransactionValueBAB)) +
  geom_col(fill = "lightblue", color = "black", width=0.5) +
  labs(title = "Giá Trị Giao Dịch Trung Bình Theo Thứ Trong Tháng 5 của BAB",
       x = "Thứ trong Tuần",
       y = "Giá Trị Giao Dịch Trung Bình (BAB)",
       fill = "Ngân hàng") +
  theme_minimal()

#################################################################################################################

# Vẽ đồ thị Probability Distributions
ggplot(data=data, aes(x = ACB.HM.Close)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Phân Phối Xác Suất của ACB.HM.Close",
       x = "Giá Đóng Cửa",
       y = "Mật Độ Xác Suất") +
  theme_minimal()


ggplot(data=data, aes(x = ACB.HM.Volume)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Phân Phối Xác Suất của ACB.HM.Volume",
       x = "Khối lượng giao dịch",
       y = "Mật Độ Xác Suất") +
  theme_minimal()


ggplot(data=data, aes(x = BAB.HN.Close)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Phân Phối Xác Suất của BAB.HN.Close",
       x = "Giá Đóng Cửa",
       y = "Mật Độ Xác Suất") +
  theme_minimal()


ggplot(data=data, aes(x = BAB.HN.Volume)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Phân Phối Xác Suất của BAB.HN.Volume",
       x = "Khối lượng giao dịch",
       y = "Mật Độ Xác Suất") +
  theme_minimal()

#################################################################################################################
