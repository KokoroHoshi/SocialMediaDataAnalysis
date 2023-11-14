library(pacman)
p_load("dplyr", "ggplot2", "this.path", "stringr", "stringdist", "tidyr", "purrr")

setwd(file.path(file.path(dirname(this.path()), ".."), ".."))
getwd()



# --- yt_data preprocess ---
yt_data <- read.csv("./data/YouTube/Hololive.csv")

yt_data <- unique(yt_data)

# change form of tags
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

names(yt_data)[names(yt_data) == "publishedDate"] <- "date"
yt_data$date <- as.Date(yt_data$date)

View(yt_data)


# --- x_data preprocess ---
x_data_1 <- read.csv("./data/X_Twitter/tweets_data_1.csv")
x_data_2 <- read.csv("./data/X_Twitter/tweets_data_2.csv")
x_data_3 <- read.csv("./data/X_Twitter/tweets_data_3.csv")
x_data_4 <- read.csv("./data/X_Twitter/tweets_data_4.csv")
x_data <- rbind(x_data_1, x_data_2, x_data_3, x_data_4)
x_data <- unique(x_data)

# reset index
rownames(x_data) <- NULL

# pick date from 2022-01-01 to 2022-12-31
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")
x_data <- x_data %>%
  filter(posted_time >= start_date & posted_time <= end_date)

names(x_data)[names(x_data) == "posted_time"] <- "date"
x_data$date <- as.Date(x_data$date)

View(x_data)



# --- find top 5 common tags ---

# count all tags
pattern <- "#[^#]+"
yt_tags <- unlist(str_extract_all(yt_data$tags, pattern))
x_tags <- unlist(str_extract_all(x_data$tags, pattern))
all_tags <- c(yt_tags, x_tags)
tag_frequency <- table(all_tags)

View(tag_freq)

tag_freq <- as.data.frame(tag_frequency)
tag_freq <- tag_freq[order(-tag_freq$Freq), ]

View(tag_freq)

tag_freq$all_tags <- as.character(tag_freq$all_tags)
top_tags <- tag_freq$all_tags[1:5]

View(top_tags)



# --- count matched num with tags (3 common tags) ---
yt_data$matched_x_nums <- 0

pattern <- "#[^#]+"

for (yt_index in seq_len(nrow(yt_data))) {
  for (x_index in seq_len(nrow(x_data))) {
    if (x_data$date[x_index] > yt_data$date[yt_index]) {
      next
    }
    
    yt_tags <- str_extract_all(yt_data$tags[yt_index], pattern)[[1]]
    x_tags <- str_extract_all(x_data$tags[x_index], pattern)[[1]]
    
    yt_tags <- gsub(" ", "", yt_tags)
    x_tags <- gsub(" ", "", x_tags)
    
    if (length(yt_tags) == 0 || length(x_tags) == 0) {
      next
    }
    
    common_tags <- intersect(yt_tags, x_tags)
    
    base_count_of_common_tags <- floor((length(x_tags)+1)/2)
    
    if (length(common_tags) >= base_count_of_common_tags && all(!common_tags %in% top_tags)) {
      yt_data$matched_x_nums[yt_index] <- yt_data$matched_x_nums[yt_index] + 1
    }
  }
}

View(yt_data)



# --- fuzzy matching with tags ---

# fuzzy matching with filtering top 5 common tags
merged_data <- x_data %>%
  rowwise() %>%
  mutate(matched_tag = {
    non_top_tags <- setdiff(yt_data$tags, top_tags)
    nearest_tag <- non_top_tags[which.min(stringdistmatrix(tags, non_top_tags))]
    nearest_tag
  })
merged_data <- left_join(yt_data, merged_data, by = c("tags" = "matched_tag"))

matched_x <- filter(merged_data, !is.na(merged_data$content))
non_matched_x <- filter(merged_data, is.na(merged_data$content))

View(matched_x)

# unique matched_x by title
matched_x <- distinct(matched_x, title, .keep_all = TRUE)

View(matched_x)

processed_data <- left_join(x_data, yt_data, by="date")
View(processed_data)


dev.off()
cat('\014')
