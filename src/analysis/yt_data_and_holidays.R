library(pacman)
p_load("dplyr", "ggplot2", "this.path")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()


# yt_data <- read.csv("./data/YouTube/CalliopeMori.csv")
yt_data <- read.csv("./data/YouTube/Hololive.csv")
JP_holidays <- read.csv("./data/Google Calendar/JP_holidays.csv")
USA_holidays <- read.csv("./data/Google Calendar/USA_holidays.csv")
ID_holidays <- read.csv("./data/Google Calendar/ID_holidays.csv")
View(yt_data)

# rbind holidays from different contries
# holidays <- rbind(JP_holidays, USA_holidays)
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

# join yt_data and holidays by date
names(yt_data)[names(yt_data) == "publishedDate"] <- "date"
yt_data$date <- as.Date(yt_data$date)
new_yt_data <- left_join(yt_data, holidays, by="date")
View(new_yt_data)

# filter new_yt_data by date
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")
new_yt_data <- new_yt_data %>%
  filter(date >= start_date & date <= end_date)

# group data on non holiday or holiday
non_holiday_yt_data <- new_yt_data %>%
  filter(is.na(holiday.name))
holiday_yt_data <- new_yt_data %>%
  filter(!is.na(holiday.name))
View(holiday_yt_data)

# views box plot for non holiday and holiday
par(mfrow = c(1, 2))
boxplot(non_holiday_yt_data$views, main = "non holiday")
boxplot(holiday_yt_data$views, main = "holiday")
par(mfrow = c(1, 1))

# filter outlier of views
IQR(non_holiday_yt_data$views)
IQR(holiday_yt_data$views)
RemoveOutliers <- function(data, column_name, multiplier = 1.5) {
  IQR_val <- IQR(column_name)
  
  lower_threshold <- quantile(column_name, 0.25) - multiplier * IQR_val
  upper_threshold <- quantile(column_name, 0.75) + multiplier * IQR_val
  
  filtered_data <- data %>%
    filter(column_name >= lower_threshold, column_name <= upper_threshold)
  
  return(filtered_data)
}
non_holiday_yt_data <- RemoveOutliers(non_holiday_yt_data, non_holiday_yt_data$views)
holiday_yt_data <- RemoveOutliers(holiday_yt_data, holiday_yt_data$views)

# views box plot for non holiday and holiday without outliers
par(mfrow = c(1, 2))
boxplot(non_holiday_yt_data$views, main = "non holiday")
boxplot(holiday_yt_data$views, main = "holiday")
par(mfrow = c(1, 1))

# summary views, like and comments of two data
summary(non_holiday_yt_data %>% select(views:comments))
summary(holiday_yt_data %>% select(views:comments))


dev.off()
cat('\014')
