gc()
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, jsonlite)

url <- "https://api.reddit.com/r/nba/top/?t=year&limit=100"
data <- fromJSON(url)
write_json(data, "data/nba-reddit/nba-top-100-threads-this-year.json")

glimpse(data)

data <- tibble(
    title = data$data$children$data$title,
    upvote_ratio = data$data$children$data$upvote_ratio
) %>%
    mutate(upvote_ratio = case_when(
        upvote_ratio >= 0.95 ~ "highly top rated",
        upvote_ratio >= 0.90 & upvote_ratio < 0.95 ~ "very top rated",
        upvote_ratio >= 0.80 & upvote_ratio < 0.90 ~ "somewhat top rated",
        upvote_ratio < 0.80 ~ "top rated"
    ))

write_csv(data, "data/nba-reddit/nba-top-100-threads-this-year-cleaned.csv")
