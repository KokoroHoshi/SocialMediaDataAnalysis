library(pacman)
p_load("dplyr", "ggplot2", "this.path")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()



# --- x_data preprocess ---
x_data_1 <- read.csv("./data/X_Twitter/tweets_data_1.csv")
x_data_2 <- read.csv("./data/X_Twitter/tweets_data_2.csv")
x_data_3 <- read.csv("./data/X_Twitter/tweets_data_3.csv")
x_data_4 <- read.csv("./data/X_Twitter/tweets_data_4.csv")
x_data <- rbind(x_data_1, x_data_2, x_data_3, x_data_4)

# unique x_data
x_data <- unique(x_data)

# change posted_time to date
names(x_data)[names(x_data) == "posted_time"] <- "date"
x_data$date <- as.Date(x_data$date)

# delete x_data if views == -1
x_data <- x_data[x_data$views != -1, ]

View(x_data)



# --- holidays preprocess ---
JP_holidays <- read.csv("./data/Google Calendar/JP_holidays.csv")
USA_holidays <- read.csv("./data/Google Calendar/USA_holidays.csv")
ID_holidays <- read.csv("./data/Google Calendar/ID_holidays.csv")

# rbind holidays from different contries
holidays <- rbind(JP_holidays, USA_holidays, ID_holidays)
View(holidays)

# sort by date
holidays$date <- as.Date(holidays$date)
sorted_idx <- order(holidays$date)
holidays <- holidays[sorted_idx, ]

# unique holidays
holidays <- unique(holidays)

# reset index
rownames(holidays) <- NULL



# --- join x_data and holidays by date ---
new_x_data <- left_join(x_data, holidays, by="date")
View(new_x_data)

# filter new_x_data by date
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")
new_x_data <- new_x_data %>%
  filter(date >= start_date & date <= end_date)

# group data on non holiday or holiday
non_holiday_x_data <- new_x_data %>%
  filter(is.na(holiday.name))
holiday_x_data <- new_x_data %>%
  filter(!is.na(holiday.name))
View(holiday_x_data)

# views box plot for non holiday and holiday
par(mfrow = c(1, 2))
par(mar = c(2, 2, 2, 2))
boxplot(non_holiday_x_data$views, main = "non holiday")
boxplot(holiday_x_data$views, main = "holiday")
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(1, 1))

# filter outlier of views
IQR(non_holiday_x_data$views)
IQR(holiday_x_data$views)
RemoveOutliers <- function(data, column_name, multiplier = 1.5) {
  IQR_val <- IQR(column_name)
  
  lower_threshold <- quantile(column_name, 0.25) - multiplier * IQR_val
  upper_threshold <- quantile(column_name, 0.75) + multiplier * IQR_val
  
  filtered_data <- data %>%
    filter(column_name >= lower_threshold, column_name <= upper_threshold)
  
  return(filtered_data)
}
non_holiday_x_data <- RemoveOutliers(non_holiday_x_data, non_holiday_x_data$views)
holiday_x_data <- RemoveOutliers(holiday_x_data, holiday_x_data$views)

# views box plot for non holiday and holiday without outliers
par(mfrow = c(1, 2))
par(mar = c(2, 2, 2, 2))
boxplot(non_holiday_x_data$views, main = "non holiday")
boxplot(holiday_x_data$views, main = "holiday")
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(1, 1))

# summary replies, retweets, likes and views of two data
summary(non_holiday_x_data %>% select(replies:views))
summary(holiday_x_data %>% select(replies:views))



dev.off()
cat('\014')
