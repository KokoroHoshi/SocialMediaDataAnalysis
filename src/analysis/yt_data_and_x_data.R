library(pacman)
p_load("dplyr", "ggplot2", "this.path", "stringr", "stringdist")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()



# yt_data preprocess
yt_data <- read.csv("./data/YouTube/Hololive.csv")
View(yt_data)
adjust_tags <- function(tags_str) {
  tags_str <- gsub("^\\[|\\]$", "", tags_str)
  tags_str <- gsub("'", "", tags_str)
  tags_str <- gsub(",\\s*", ",", tags_str)
  adjusted_tags <- str_extract_all(tags_str, "[^,]+")
  adjusted_tags <- sapply(adjusted_tags, paste, collapse = " #")
  adjusted_tags <- sapply(adjusted_tags, function(tag) ifelse(startsWith(tag, "#"), tag, paste("#", tag, sep = "")), USE.NAMES = FALSE)
  
  return(adjusted_tags)
}
yt_data$tags <- apply(yt_data, 1, function(row) adjust_tags(row["tags"]))

# pick date from 2022-01-01 to 2022-12-31
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")
yt_data <- yt_data %>%
  filter(publishedDate >= start_date & publishedDate <= end_date)



# x_data preprocess
x_data_1 <- read.csv("./data/X_Twitter/tweets_data_1.csv")
x_data_2 <- read.csv("./data/X_Twitter/tweets_data_2.csv")
x_data_3 <- read.csv("./data/X_Twitter/tweets_data_3.csv")
x_data_4 <- read.csv("./data/X_Twitter/tweets_data_4.csv")
x_data <- rbind(x_data_1, x_data_2, x_data_3, x_data_4)
x_data <- unique(x_data)
View(x_data)

# reset index
rownames(x_data) <- NULL

# pick date from 2022-01-01 to 2022-12-31
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")
x_data <- x_data %>%
  filter(posted_time >= start_date & posted_time <= end_date)


# filter common tags

# fuzzy matching with tags and date


# join x_data and yt_data by tags
# merged_data <- left_join(yt_data, x_data, by="tags")
# fuzzy matching (need to filter common tags first)
# 1
merged_data <- x_data %>%
  rowwise() %>%
  mutate(matched_tag = yt_data$tags[which.min(stringdistmatrix(tags, yt_data$tags))])

merged_data <- left_join(yt_data, merged_data, by = c("tags" = "matched_tag"))

# 2
merged_data <- x_data %>%
  mutate(matched_tag = yt_data$tags[which.min(stringdistmatrix(tags, yt_data$tags))])
View(merged_data)
merged_data <- left_join(yt_data, merged_data, by = c("tags" = "matched_tag"))


View(merged_data)


names(x_data)[names(x_data) == "posted_time"] <- "date"
names(yt_data)[names(yt_data) == "publishedDate"] <- "date"
yt_data$date <- as.Date(yt_data$date)
processed_data <- left_join(x_data, yt_data, by="date")
View(processed_data)


dev.off()
cat('\014')