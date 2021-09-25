library(RedditExtractoR)
library(tidyverse)
library(tidytext)

top_nba_daily <- find_thread_urls(subreddit = "nba", sort_by = "top", period = "day")
str(top_nba_daily)
threads_contents <- get_thread_content(top_nba_daily$url[1:10]) # time-consuming

# save(threads_contents, file = paste0("data/nba-reddit/", Sys.Date(), "-top10.RDS"))

threads <- threads_contents$threads %>%
    as_tibble() %>%
    select(-c(author, date, subreddit, upvotes, downvotes,
              total_awards_received, golds, cross_posts))

comments <- threads_contents$comments %>%
    as_tibble() %>%
    select(-c(url, author, date, upvotes, downvotes, golds))
rm(threads_contents)

text_df <- tibble(line=1:(2*nrow(threads)+nrow(comments)),
                  text=c(threads$title, threads$text, comments$comment))

tidy_text <- text_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

tidy_text <- tidy_text %>%
    drop_na() %>%
    count(word, sort=TRUE) %>%
    mutate(word = str_extract(word, "[a-zA-Z0-9']+"))

write_csv(tidy_text, paste0("data/nba-reddit/", Sys.Date(), "-tidy-text.csv"))