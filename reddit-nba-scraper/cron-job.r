library(RedditExtractoR)

top_nba_daily <- find_thread_urls(subreddit = "nba", sort_by = "top", period = "day")
str(top_nba_daily)
threads_contents <- get_thread_content(top_nba_daily$url[1:10]) # time-consuming

save(threads_contents, file = paste0(Sys.Date(), "-top10.RDS"))
