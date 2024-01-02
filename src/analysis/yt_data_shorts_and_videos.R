library(pacman)
p_load("dplyr", "ggplot2", "this.path")
p_load("cluster", "factoextra", "NbClust")
p_load("lubridate", "stringr")
p_load("rpart", "rpart.plot", "caret")
p_load("car")
p_load("nnet", "pscl")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()

# --- preprocess ---

yt_data <- read.csv("./data/YouTube/new_Hololive.csv")

# transform variables
# title -> title_num
yt_data$title_num <- nchar(yt_data$title)

# publishedDate -> published_hour
# yt_data$publishedTime <- as.POSIXct(yt_data$publishedDate, 
#                                     format="%Y-%m-%dT%H:%M:%S", 
#                                     tz="UTC")
# yt_data$publishedTime <- format(yt_data$publishedTime,
#                                 format = "%H:%M")
yt_data$published_hour <- hour(as.POSIXct(yt_data$publishedDate, 
                                          format="%Y-%m-%dT%H:%M:%S", 
                                          tz="UTC"))

hist(yt_data$published_hour, breaks = 24)

# duration -> duration_sec
# extract numeric parts for each unit
hms <- sapply(c('H', 'M', 'S'), function(unit) 
  sub(paste0('.*[^0-9]+([0-9]+)', unit, '.*'), '\\1', yt_data$duration))
# change strings to numbers (non-numbers become NA)
suppressWarnings(mode(hms) <- 'numeric')
# multiply by seconds (3600, 60, 1) and sum
yt_data$duration_sec <- colSums(t(hms) * 60^(2:0), na.rm=T)

# tags -> tag_num
adjust_tags <- function(tags_str) {
  tags_str <- gsub("^\\[|\\]$", "", tags_str)
  if (tags_str == "") return(NULL)
  
  tags_str <- gsub("'", "", tags_str)
  tags_str <- gsub(",\\s*", ",", tags_str)
  adjusted_tags <- str_extract_all(tags_str, "[^,]+")
  adjusted_tags <- sapply(adjusted_tags, paste, collapse = " #")
  adjusted_tags <- sapply(adjusted_tags,
                          function(tag) ifelse(startsWith(tag, "#"),
                                               tag,
                                               paste("#", tag, sep = "")),
                          USE.NAMES = FALSE)
  
  return(adjusted_tags)
}

yt_data$tags_processed <- apply(yt_data, 1, function(row) adjust_tags(row["tags"]))

tags_extracted <- str_extract_all(yt_data$tags_processed, "#[^#]+")

yt_data$tag_num <- sapply(tags_extracted, length)

# filter date, privacyStatus, NA, and live-streams
names(yt_data)[names(yt_data) == "publishedDate"] <- "date"
end_date <- as.Date("2023-12-01")
new_yt_data <- yt_data %>%
  filter(date < end_date) %>%
  filter(privacyStatus == "public") %>%
  filter(liveStreamScheduledStartTime == "")

na_yt_data <- new_yt_data[rowSums(is.na(new_yt_data)) > 0, ]

new_yt_data <- na.omit(new_yt_data)

# feature scaling
scaled_yt_data <- new_yt_data %>%
  mutate(across(views:comments, ~scale(.)[, 1],  .names = "{col}_scaled"))

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
                          centers = 3,
                          iter.max = 30,
                          nstart = 25)

fviz_cluster(cluster_results,
             data = cluster_data)

scaled_yt_data$cluster <- cluster_results$cluster

# rename cluster (level: 1 > 2 > 3)
scaled_yt_data <- scaled_yt_data %>%
  mutate(cluster = dplyr::recode(cluster, '1' = '1', '2' = '3', '3' = '2'))

hist(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)


# --- decision tree (acc = 0.8942308) ---

analysis_data <- scaled_yt_data %>%
  select(title_num:duration_sec,
         tag_num,
         cluster)

set.seed(563)

train.index <- sample(x = 1:nrow(analysis_data),
                      size = ceiling(0.8 * nrow(analysis_data)))

train <- analysis_data[train.index, ]
test <- analysis_data[-train.index, ]

tree_model <- rpart(cluster ~ .,
                    data = train,
                    method = "class")

summary(tree_model)

rpart.plot(tree_model)

prp(tree_model,
    faclen = 0,
    fallen.leaves = TRUE,
    shadow.col = "gray", 
    extra = 3)

tree_pred <- predict(tree_model,
                     newdata = test,
                     type = "class")

table(real = test$cluster, predict = tree_pred)
confus.matrix <- table(real = test$cluster, predict = tree_pred)
confus.matrix

sum(tree_pred == test$cluster) / NROW(tree_pred)


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
scaled_yt_data <- scaled_yt_data %>%
  mutate(cluster = dplyr::recode(cluster, '1' = '2', '2' = '1', '3' = '3'))

scaled_yt_data$cluster <- as.numeric(scaled_yt_data$cluster)

hist(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)


# --- decision tree again (acc = 0.7281553) ---

analysis_data <- scaled_yt_data %>%
  select(title_num:duration_sec,
         tag_num,
         cluster)

set.seed(563)

train.index <- sample(x = 1:nrow(analysis_data),
                      size = ceiling(0.8 * nrow(analysis_data)))

train <- analysis_data[train.index, ]
test <- analysis_data[-train.index, ]

tree_model <- rpart(cluster ~ .,
                    data = train,
                    method = "class")

summary(tree_model)

rpart.plot(tree_model)

prp(tree_model,
    faclen = 0,
    fallen.leaves = TRUE,
    shadow.col = "gray", 
    extra = 2)

tree_pred <- predict(tree_model,
                     newdata = test,
                     type = "class")

table(real = test$cluster, predict = tree_pred)
confus.matrix <- table(real = test$cluster, predict = tree_pred)
confus.matrix
sum(tree_pred == test$cluster) / NROW(tree_pred)


# --- prune tree (acc = 0.6796117)---

pruned_tree_model <- prune(tree_model, cp = 0.05)

summary(pruned_tree_model)

rpart.plot(pruned_tree_model)

prp(pruned_tree_model,
    faclen = 0,
    fallen.leaves = TRUE,
    shadow.col = "gray", 
    extra = 3)

tree_pred <- predict(pruned_tree_model,
                     newdata = test,
                     type = "class")

table(real = test$cluster, predict = tree_pred)
confus.matrix <- table(real = test$cluster, predict = tree_pred)
confus.matrix

sum(tree_pred == test$cluster) / NROW(tree_pred)

# --- correlation coefficient ---

analysis_data <- scaled_yt_data %>%
  select(title_num:duration_sec,
         tag_num,
         cluster)

cor(analysis_data, method = "pearson")
cor(analysis_data, method = "spearman")
cor(analysis_data, method = "kendall")


# --- linear regresstion (acc = 0.6019417)---

ln_model <- lm(cluster ~ ., data = train)

summary(ln_model)

avPlots(ln_model)

ln_pred <- round(predict(ln_model, newdata = test))

sum(ln_pred == test$cluster) / NROW(ln_pred)

xtabs(~ ln_pred + test$cluster)


# --- multinomial logistic regression (acc = 0.7475728) ---

set.seed(563)

(log_model <- multinom(cluster ~ ., data = train))

summary(log_model)

pR2(log_model)

log_pred <- predict(log_model, newdata = test)

sum(log_pred == test$cluster) / NROW(log_pred)

xtabs(~ log_pred + test$cluster)

