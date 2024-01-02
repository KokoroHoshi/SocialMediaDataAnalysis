library(pacman)
p_load("dplyr", "ggplot2", "this.path")
p_load("stringr", "stringdist", "tidyr", "purrr")
p_load("cluster", "factoextra", "NbClust")
p_load("fuzzyjoin", "lubridate")
p_load("MASS")
p_load("caret")
p_load("sampling")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()


# --- yt_data preprocess ---

yt_data <- read.csv("./data/YouTube/new_Hololive.csv")

end_date <- as.Date("2023-12-01")
new_yt_data <- yt_data %>%
  filter(publishedDate < end_date)

# check missing data
na_yt_data <- new_yt_data[rowSums(is.na(new_yt_data)) > 0, ]

new_yt_data <- na.omit(new_yt_data)

# feature scaling
partial_yt_data <- new_yt_data %>% 
  dplyr::select(id, title:duration, views:comments) 

scaled_yt_data <- partial_yt_data %>%
  mutate(across(views:comments, ~scale(.)[, 1],  .names = "{col}_scaled"))

# clustering
cluster_data <- scaled_yt_data %>% 
  dplyr::select(views_scaled:comments_scaled)

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

scaled_yt_data$cluster <- cluster_results$cluster

hist(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)

summary(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
summary(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
summary(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)

# rename cluster (level: 1 > 2 > 3)
scaled_yt_data <- scaled_yt_data %>%
  mutate(cluster = recode(cluster, '1' = '2', '2' = '1', '3' = '3'))

hist(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
hist(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)

summary(scaled_yt_data[scaled_yt_data$cluster == 1, ]$views)
summary(scaled_yt_data[scaled_yt_data$cluster == 2, ]$views)
summary(scaled_yt_data[scaled_yt_data$cluster == 3, ]$views)


# --- x_data preprocess ---

file_paths <- list.files(path = "./data/X_Twitter/", pattern = "tweets_data_\\d\\.csv", full.names = TRUE)
x_data <- do.call(rbind, lapply(file_paths, read.csv))

x_data <- unique(x_data)

end_date <- as.Date("2023-12-01")
x_data <- x_data %>%
  filter(posted_time < end_date)

partial_x_data <- x_data[, c("content", "posted_time")]


# --- fuzzy matching with title and content ---

matching_result <- stringdist_join(scaled_yt_data, partial_x_data, 
                        by=c('title'='content'),
                        mode='left',
                        method = "jw",
                        max_dist=0.45, 
                        distance_col='dist') %>%
  arrange(desc(dist))

# as.Date("2021-12-01") - as.Date("2020-12-01")

filtered_result <- matching_result %>%
  filter(publishedDate > posted_time,
         as.Date(publishedDate) - as.Date(posted_time) <= 30) 

matched_sum <- filtered_result %>%
  group_by(title) %>%
  mutate(matched_num = n()) %>%
  distinct(title, matched_num, .keep_all = TRUE) %>%
  subset(select = -c(content, posted_time, dist))

matched_yt_data <- merge(scaled_yt_data, matched_sum, by = "id", all.x = TRUE) %>%
  dplyr::select(-contains(".y"))

matched_yt_data$matched_num[is.na(matched_yt_data$matched_num)] <- 0

matched_yt_data$matched <- matched_yt_data$matched_num > 0


# --- analysis ---

# view difference between matched and unmatched
table(matched_yt_data$matched_num)

hist(matched_yt_data$matched_num)
hist(matched_yt_data$matched_num[matched_yt_data$matched_num > 0])

matched <- matched_yt_data[matched_yt_data$matched_num > 0, ]
unmatched <- matched_yt_data[matched_yt_data$matched_num == 0, ]

# PCA
summary(princomp(matched_yt_data %>% dplyr::select(views_scaled.x:comments_scaled.x)))

hist(matched$views.x)
hist(unmatched$views.x)

hist(matched$views_scaled.x)
hist(unmatched$views_scaled.x)

summary(matched$views.x)
summary(unmatched$views.x)

summary(matched$views_scaled.x)
summary(unmatched$views_scaled.x)

# independent testing
cross_table <- xtabs(~cluster.x + matched_num, data=matched_yt_data)
cross_table

TF_cross_table <- xtabs(~cluster.x + matched, data = matched_yt_data)
TF_cross_table

chisq.test(TF_cross_table)

fisher.test(TF_cross_table)

# correlation coefficient (unbalance?)
analysis_data <- matched_yt_data %>% 
  dplyr::select(views_scaled.x:comments_scaled.x, cluster.x, matched_num, matched)

analysis_data$matched <- as.numeric(analysis_data$matched)
analysis_data$cluster.x <- as.numeric(analysis_data$cluster.x)

cor(analysis_data, method = "pearson")
cor(analysis_data, method = "spearman")
cor(analysis_data, method = "kendall")

# up sampling
cross_table <- xtabs(~cluster.x + matched, data = matched_yt_data)
cross_table

set.seed(563)
sampled_data <- upSample(matched_yt_data, as.factor(matched_yt_data$matched))

sampled_cross_table <- xtabs(~cluster.x + matched, data = sampled_data)
sampled_cross_table

sampled_cross_table <- xtabs(~cluster.x + matched_num, data = sampled_data)
sampled_cross_table

matched <- sampled_data[sampled_data$matched_num > 0, ]
unmatched <- sampled_data[sampled_data$matched_num == 0, ]

hist(matched$views.x)
hist(unmatched$views.x)

summary(matched$views.x)
summary(unmatched$views.x)

# correlation coefficient again
analysis_data <- sampled_data %>% 
  dplyr::select(views_scaled.x:comments_scaled.x, cluster.x, matched_num, matched)

analysis_data$matched <- as.numeric(analysis_data$matched)
analysis_data$cluster.x <- as.numeric(analysis_data$cluster.x)

cor(analysis_data, method = "pearson")
cor(analysis_data, method = "spearman")
cor(analysis_data, method = "kendall")

# down sampling
cross_table <- xtabs(~cluster.x + matched, data = matched_yt_data)
cross_table

set.seed(563)
sampled_data <- downSample(matched_yt_data, as.factor(matched_yt_data$matched))

sampled_cross_table <- xtabs(~cluster.x + matched, data = sampled_data)
sampled_cross_table

sampled_cross_table <- xtabs(~cluster.x + matched_num, data = sampled_data)
sampled_cross_table

matched <- sampled_data[sampled_data$matched_num > 0, ]
unmatched <- sampled_data[sampled_data$matched_num == 0, ]

hist(matched$views.x)
hist(unmatched$views.x)

summary(matched$views.x)
summary(unmatched$views.x)

# correlation coefficient again
analysis_data <- sampled_data %>% 
  dplyr::select(views_scaled.x:comments_scaled.x, cluster.x, matched_num, matched)

analysis_data$matched <- as.numeric(analysis_data$matched)
analysis_data$cluster.x <- as.numeric(analysis_data$cluster.x)

cor(analysis_data, method = "pearson")
cor(analysis_data, method = "spearman")
cor(analysis_data, method = "kendall")

# distribution testing
shapiro.test(analysis_data$views_scaled.x)
# p_value < 0.05 -> non-normal distribution
# p_value < 0.003 ->  can use Box Cox

# linear regression (y = views.x)
view_and_matched_model <- lm(views.x ~ matched_num, data = matched_yt_data)
plot(view_and_matched_model)
summary(view_and_matched_model)

bc <- MASS::boxcox(view_and_matched_model)
best_lambda <- bc$x[which(bc$y == max(bc$y))]

view_and_matched_model <- lm((views.x)^best_lambda ~ matched_num, data = matched_yt_data)
plot(view_and_matched_model)
summary(view_and_matched_model)

plot(matched_yt_data$matched_num, matched_yt_data$views.x)
abline(coef(view_and_matched_model))

predict(view_and_matched_model,
        newdata = data.frame(matched_num = seq(0, 10)))

# linear regression (y = cluster.x)
model <- lm(as.numeric(cluster.x) ~ matched_num, data = matched_yt_data)
summary(model)

bc <- MASS::boxcox(model)
best_lambda <- bc$x[which(bc$y == max(bc$y))]

model <- lm((as.numeric(cluster.x))^best_lambda ~ matched_num, data = matched_yt_data)
summary(model)

plot(matched_yt_data$matched_num, matched_yt_data$cluster.x)
abline(coef(model))

predict(model, newdata = data.frame(matched_num = seq(0, 10)))

# stratified sampling with "matched"
cross_table <- xtabs(~cluster.x + matched, data = matched_yt_data)
cross_table

hist(matched_yt_data$views.x)

set.seed(563)
st <- strata(c("matched"),
             size = c(100, 100),
             method = "srswor",
             data = matched_yt_data)

sampled_data <- getdata(matched_yt_data, st)

sampled_cross_table <- xtabs(~cluster.x + matched, data = sampled_data)
sampled_cross_table

hist(sampled_data$views.x)

matched <- sampled_data[sampled_data$matched_num > 0, ]
unmatched <- sampled_data[sampled_data$matched_num == 0, ]

hist(matched$views.x)
hist(unmatched$views.x)

summary(matched$views.x)
summary(unmatched$views.x)

# stratified sampling with "matched" and "cluster.x"
analysis_data <- matched_yt_data[matched_yt_data$cluster.x > 1, ]

cross_table <- xtabs(~cluster.x + matched, data = analysis_data)
cross_table

set.seed(563)
st <- strata(c("matched", "cluster.x"),
             size = c(100, 300, 100, 300),
             method = "srswr",
             data = analysis_data)

sampled_data <- getdata(analysis_data, st)

sampled_cross_table <- xtabs(~cluster.x + matched, data = sampled_data)
sampled_cross_table

hist(sampled_data$views.x)

matched <- sampled_data[sampled_data$matched_num > 0, ]
unmatched <- sampled_data[sampled_data$matched_num == 0, ]

hist(matched$views.x)
hist(unmatched$views.x)

summary(matched$views.x)
summary(unmatched$views.x)

# correlation coefficient again
analysis_data <- sampled_data %>% 
  dplyr::select(views_scaled.x:comments_scaled.x, cluster.x, matched_num, matched)

analysis_data$matched <- as.numeric(analysis_data$matched)
analysis_data$cluster.x <- as.numeric(analysis_data$cluster.x)

cor(analysis_data, method = "pearson")
cor(analysis_data, method = "spearman")
cor(analysis_data, method = "kendall")


# --- remove outliers (cluster == 1) ---

new_matched_yt_data <- matched_yt_data[matched_yt_data$cluster != 1, ]

# clustering again
cluster_data <- new_matched_yt_data[, c("views_scaled.x",
                                        "likes_scaled.x",
                                        "comments_scaled.x")]
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

new_matched_yt_data$cluster.x <- cluster_results$cluster

# rename cluster (level: 1 > 2 > 3)
new_matched_yt_data <- new_matched_yt_data %>%
  mutate(cluster.x = dplyr::recode(cluster.x, '1' = '3', '2' = '1', '3' = '2'))

new_matched_yt_data$cluster.x <- as.numeric(new_matched_yt_data$cluster.x)

hist(new_matched_yt_data[new_matched_yt_data$cluster.x == 1, ]$views.x)
hist(new_matched_yt_data[new_matched_yt_data$cluster.x == 2, ]$views.x)
hist(new_matched_yt_data[new_matched_yt_data$cluster.x == 3, ]$views.x)

# independent testing (H0 -> independent)
cross_table <- xtabs(~cluster.x + matched_num, data = new_matched_yt_data)
cross_table

TF_cross_table <- xtabs(~cluster.x + matched, data = new_matched_yt_data)
TF_cross_table

chisq.test(TF_cross_table)

fisher.test(TF_cross_table)

# correlation coefficient again
analysis_data <- new_matched_yt_data %>% 
  dplyr::select(views_scaled.x:comments_scaled.x, cluster.x, matched_num, matched)

analysis_data$matched <- as.numeric(analysis_data$matched)
analysis_data$cluster.x <- as.numeric(analysis_data$cluster.x)

cor(analysis_data, method = "pearson")
cor(analysis_data, method = "spearman")
cor(analysis_data, method = "kendall")

# distribution testing
shapiro.test(analysis_data$views_scaled.x)


# --- clean environment ---
dev.off()
cat('\014')
