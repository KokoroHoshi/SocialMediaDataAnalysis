library(pacman)
p_load("dplyr", "ggplot2", "this.path")
p_load("cluster", "factoextra", "NbClust")
p_load("vcd")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()


# --- data preprocess ---

# x_data
file_paths <- list.files(path = "./data/X_Twitter/", pattern = "tweets_data_\\d\\.csv", full.names = TRUE)
x_data <- do.call(rbind, lapply(file_paths, read.csv))

x_data <- unique(x_data)

end_date <- as.Date("2023-12-01")
x_data <- x_data %>%
  filter(posted_time < end_date) %>%
  filter(views != -1)

names(x_data)[names(x_data) == "posted_time"] <- "date"

summary(x_data %>% dplyr::select(replies:views))


# holidays
JP_holidays <- read.csv("./data/Holidays/japan_holidays.csv")
USA_holidays <- read.csv("./data/Holidays/us_holidays.csv")
ID_holidays <- read.csv("./data/Holidays/indonesia_holidays.csv")


holidays <- rbind(JP_holidays, USA_holidays, ID_holidays)

holidays_date <- holidays %>% 
  filter(type == "Observance" | 
           type == "National holiday" | 
           type == "Federal Holiday") %>%
  distinct(date) %>%
  mutate(is_holiday = TRUE)

# join data
x_data$date <- as.Date(x_data$date)
holidays_date$date <- as.Date(holidays_date$date)

new_x_data <- left_join(x_data, holidays_date, by="date")
new_x_data[is.na(new_x_data)] <- FALSE

# feature scaling
scaled_x_data <- new_x_data %>% 
  dplyr::select(replies:is_holiday) %>%
  mutate(across(replies:views, ~scale(.)[, 1],  .names = "{col}_scaled"))


# --- histogram ---

RemoveOutliers <- function(data, column_name, multiplier = 1.5) {
  col <- data[, column_name]
  
  IQR_val <- IQR(col)
  
  lower_threshold <- quantile(col, 0.25) - multiplier * IQR_val
  upper_threshold <- quantile(col, 0.75) + multiplier * IQR_val
  
  filtered_data <- data %>%
    filter(col >= lower_threshold, col <= upper_threshold)
  
  return(filtered_data)
}

par(mfrow = c(1, 4))
hist(new_x_data$retweets)
hist(new_x_data$views)
hist(new_x_data$likes)
hist(new_x_data$replies)
par(mfrow = c(1, 1))

par(mfrow = c(1, 4))
hist(RemoveOutliers(new_x_data, "retweets")$retweets)
hist(RemoveOutliers(new_x_data, "views")$views)
hist(RemoveOutliers(new_x_data, "likes")$likes)
hist(RemoveOutliers(new_x_data, "replies")$replies)
par(mfrow = c(1, 1))


# --- analysis ---

# clustering
set.seed(563)

#cluster_data <- scaled_x_data[, c("replies", "retweets", "likes", "views")]
cluster_data <- scaled_x_data %>% dplyr::select(replies_scaled:views_scaled)

fviz_nbclust(cluster_data,
             FUNcluster = kmeans,
             method = "wss",
             k.max = 12) +
  labs(title = "Elboe Method for K-Means") +
  geom_vline(xintercept = 3,
             linetype = 2)

cluster_results <- kmeans(cluster_data,
                          centers = 3,
                          iter.max = 30,
                          nstart = 25)

fviz_cluster(cluster_results,
             data = cluster_data)

cluster_results$centers

scaled_x_data$cluster <- cluster_results$cluster

hist(scaled_x_data[scaled_x_data$cluster == 1, ]$views)
hist(scaled_x_data[scaled_x_data$cluster == 2, ]$views)
hist(scaled_x_data[scaled_x_data$cluster == 3, ]$views)


# --- analysis ---

# independent testing
cross_table <- xtabs(~cluster + is_holiday, data=scaled_x_data)
cross_table

chisq.test(cross_table)

fisher.test(cross_table)

# correlation coefficient
cor(scaled_x_data[, 5:10], method = "pearson")
cor(scaled_x_data[, 5:10], method = "spearman")
cor(scaled_x_data[, 5:10], method = "kendall")

cor.test(scaled_x_data$cluster,
         as.numeric(scaled_x_data$is_holiday),
         method = "pearson")


# --- clean environment ---

dev.off()
cat('\014')
