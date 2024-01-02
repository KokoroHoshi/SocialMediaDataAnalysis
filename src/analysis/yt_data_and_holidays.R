library(pacman)
p_load("dplyr", "ggplot2", "this.path")
p_load("cluster", "factoextra", "NbClust")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()


# --- load data ---

yt_data <- read.csv("./data/YouTube/new_Hololive.csv")

# https://dev.timeanddate.com/docs/type-holidaytype
# learn more information about holiday type through the url above
JP_holidays <- read.csv("./data/Holidays/japan_holidays.csv")
USA_holidays <- read.csv("./data/Holidays/us_holidays.csv")
ID_holidays <- read.csv("./data/Holidays/indonesia_holidays.csv")


# --- data preprocess ---

# rbind holidays from different contries
holidays <- rbind(JP_holidays, USA_holidays, ID_holidays)

# table(ID_holidays$type)
# table(JP_holidays$type)
# table(USA_holidays$type)

holidays_date <- holidays %>% 
  filter(type == "Observance" | 
           type == "National holiday" | 
           type == "Federal Holiday") %>%
  distinct(date) %>%
  mutate(is_holiday = TRUE)

# count(holidays_date %>% filter(date < "2019"))


# join yt_data and holidays by date

# yt_data <- unique(yt_data)

names(yt_data)[names(yt_data) == "publishedDate"] <- "date"

yt_data$date <- as.Date(yt_data$date)
holidays_date$date <- as.Date(holidays_date$date)

new_yt_data <- left_join(yt_data, holidays_date, by="date")
new_yt_data[is.na(new_yt_data)] <- FALSE


# filter new_yt_data by date
end_date <- as.Date("2023-12-01")
new_yt_data <- new_yt_data %>%
  filter(date < end_date)


# summary
summary(new_yt_data %>% dplyr::select(views:comments))


# histogram
RemoveOutliers <- function(data, column_name, multiplier = 1.5) {
  col <- data[, column_name]
  
  IQR_val <- IQR(col)
  
  lower_threshold <- quantile(col, 0.25) - multiplier * IQR_val
  upper_threshold <- quantile(col, 0.75) + multiplier * IQR_val
  
  filtered_data <- data %>%
    filter(col >= lower_threshold, col <= upper_threshold)
  
  return(filtered_data)
}

par(mfrow = c(3, 1))
hist(new_yt_data$views)
hist(new_yt_data$likes)
hist(new_yt_data$comments)
par(mfrow = c(1, 1))

par(mfrow = c(3, 1))
hist(RemoveOutliers(new_yt_data, "views")$views)
hist(RemoveOutliers(new_yt_data, "likes")$likes)
hist(RemoveOutliers(new_yt_data, "comments")$comments)
par(mfrow = c(1, 1))


# feature scaling
scaled_yt_data <- cbind(as.data.frame(scale(new_yt_data %>% 
                                              dplyr::select(views:comments))), 
                        new_yt_data %>% dplyr::select(is_holiday))
scaled_yt_data <- new_yt_data %>%
  mutate(across(views:comments, ~scale(.)[, 1],  .names = "{col}_scaled")) %>%
  dplyr::select(views:comments, views_scaled:is_holiday)

# clustering
cluster_data <- scaled_yt_data[, c("views", "likes", "comments")]
set.seed(563)
fviz_nbclust(cluster_data,
             FUNcluster = kmeans,
             method = "wss",
             k.max = 12) +
labs(title = "Elboe Method for K-Means") +
geom_vline(xintercept = 3,
           linetype = 2)

cluster_results <- kmeans(cluster_data,
                          centers = 3)

fviz_cluster(cluster_results,
             data = cluster_data)

cluster_results$centers

scaled_yt_data$cluster <- cluster_results$cluster

hist(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)


# --- analysis ---

# independent testing
cross_table <- xtabs(~cluster + is_holiday, data=scaled_yt_data)
cross_table

chisq.test(cross_table)

fisher.test(cross_table)


# cluster with single factor only
show_best_k <- function(data) {
  set.seed(563)
  
  fviz_nbclust(data,
               FUNcluster = kmeans,
               method = "wss",
               k.max = 12) +
    labs(title = "Elboe Method for K-Means") +
    geom_vline(xintercept = 3, 
               linetype = 2)
}

clustering <- function (data, k = 3) {
  cluster_results <- kmeans(data, centers = k)
  
  data$id <- seq_len(nrow(data))
  print(fviz_cluster(cluster_results,
                     data = data,
                     geom = "point",
                     stand = FALSE,
                     ellipse = FALSE,
                     ellipse.type = "convex"))
  
  data$cluster <- cluster_results$cluster
  
  data <- cbind(data, new_yt_data %>% dplyr::select(is_holiday))
  cross_table <- xtabs(~cluster + is_holiday, data = data)
  
  print(chisq.test(cross_table))
  
  print(fisher.test(cross_table))
}

cur_data <- as.data.frame(scaled_yt_data[, "views"])
show_best_k(cur_data)
clustering(cur_data, k = 3)

cur_data <- as.data.frame(scaled_yt_data[, "likes"])
show_best_k(cur_data)
clustering(cur_data, k = 3)

cur_data <- as.data.frame(scaled_yt_data[, "comments"])
show_best_k(cur_data)
clustering(cur_data, k = 3)


# correlation coefficient
cor(scaled_yt_data[, 7:8], method = "pearson")
cor(scaled_yt_data[, 7:8], method = "spearman")
cor(scaled_yt_data[, 7:8], method = "kendall")

cor.test(scaled_yt_data$cluster,
         as.numeric(scaled_yt_data$is_holiday),
         method = "pearson")
cor.test(scaled_yt_data$cluster,
         as.numeric(scaled_yt_data$is_holiday),
         method = "spearman")
cor.test(scaled_yt_data$cluster,
         as.numeric(scaled_yt_data$is_holiday),
         method = "kendall")

# correlation coefficient with single cluster
c1_yt_data <- scaled_yt_data %>% filter(cluster == 1)
c2_yt_data <- scaled_yt_data %>% filter(cluster == 2)
c3_yt_data <- scaled_yt_data %>% filter(cluster == 3)

cor(c1_yt_data %>% dplyr::select(views:comments, is_holiday))
cor(c2_yt_data %>% dplyr::select(views:comments, is_holiday))
cor(c3_yt_data %>% dplyr::select(views:comments, is_holiday))


# --- remove outliers (cluster == 1) ---

scaled_yt_data <- scaled_yt_data[scaled_yt_data$cluster != 1, ]

# clustering again
cluster_data <- scaled_yt_data[, c("views", "likes", "comments")]
set.seed(563)
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

scaled_yt_data$cluster <- cluster_results$cluster

# rename cluster (level: 1 > 2 > 3)
#scaled_yt_data <- scaled_yt_data %>%
#  mutate(cluster = dplyr::recode(cluster, '1' = '3', '2' = '1', '3' = '2'))

scaled_yt_data$cluster <- as.numeric(scaled_yt_data$cluster)

hist(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)

# independent testing
cross_table <- xtabs(~cluster + is_holiday, data=scaled_yt_data)
cross_table

chisq.test(cross_table)

fisher.test(cross_table)

# correlation coefficient
cor(scaled_yt_data[, 7:8], method = "pearson")
cor(scaled_yt_data[, 7:8], method = "spearman")
cor(scaled_yt_data[, 7:8], method = "kendall")

cor.test(scaled_yt_data$cluster,
         as.numeric(scaled_yt_data$is_holiday),
         method = "pearson")
cor.test(scaled_yt_data$cluster,
         as.numeric(scaled_yt_data$is_holiday),
         method = "spearman")
cor.test(scaled_yt_data$cluster,
         as.numeric(scaled_yt_data$is_holiday),
         method = "kendall")


# --- clean environment ---
dev.off()
cat('\014')