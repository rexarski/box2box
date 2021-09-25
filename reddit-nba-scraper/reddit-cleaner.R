gc()
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, tm, ggthemes)

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
    
    reddit_posts <- c(threads$title, threads$text, comments$comment)
    str(reddit_posts)
    
    reddit_source <- VectorSource(reddit_posts)
    
    # Make a volatile corpus: reddit_corpus
    reddit_corpus <- VCorpus(reddit_source)
    # Print out the reddit_corpus
    reddit_corpus
    
    # reddit_corpus[[42]]
    # reddit_corpus[[42]][1]
    # str(reddit_corpus[[42]])
    
    clean_corpus <- function(corpus) {
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, stopwords("en"))
        return(corpus)
    }
    
    clean_corpus <- clean_corpus(reddit_corpus)
    
    
    tidy_text <- tidy(clean_corpus, collapse = "\n")
    
    tidy_text <- tidy_text %>%
        select(text) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)
    
    tidy_text <- tidy_text %>%
        drop_na() %>%
        count(word, sort=TRUE) %>%
        mutate(word = str_extract(word, "[a-zA-Z0-9']+"))
        
    write_csv(tidy_text, str_replace(f, "top10.RDS", "tidy-text.csv"))
}


