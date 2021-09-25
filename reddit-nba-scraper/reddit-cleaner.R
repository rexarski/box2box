gc()
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, ggthemes)

files <- list.files("data/nba-reddit", full.names = T, pattern = ".RDS")

for (f in files) {
    load(f)
    
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
        
    write_csv(tidy_text, str_replace(f, "top10.RDS", "tidy-text.csv"))
}


