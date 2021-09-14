if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, jsonlite, RedditExtractoR)

secrets <- read_csv("reddit-nba-scraper/RedditAPIAuth.txt")

client_id <- secrets$client_id
client_secret <- secrets$client_secret
app_name <- secrets$app_name
reddit_username <- secrets$reddit_username
reddit_password <- secrets$reddit_password

response <- POST("https://www.reddit.com/api/v1/access_token",
                 authenticate(client_id, client_secret),
                 user_agent(app_name),
                 body = list(grant_type="password", 
                             username=reddit_username, 
                             password=reddit_password))
access_token_json <- rawToChar(response$content)
access_token_content <- fromJSON(access_token_json)
access_token <- access_token_content$access_token
access_token

url <- "https://oauth.reddit.com/api/v1/me" # try api/v1/me
# url <- "https://oauth.reddit.com/api/r/nba/top"

authorization_bearer <- paste("Bearer ", access_token, sep="")
result <- GET(url, 
              user_agent("APP_NAME"), 
              add_headers(Authorization = authorization_bearer))
result_json <- rawToChar(result$content)
result_content <- fromJSON(result_json)
result_content

top_nba_daily <- find_thread_urls(subreddit = "nba", sort_by = "top", period = "day")
str(top_nba_daily)
threads_contents <- get_thread_content(top_nba_daily$url[1:10]) # time-consuming
str(threads_contents$threads) # thread metadata
str(threads_contents$comments)

save(threads_contents, file = paste0("data/nba-reddit/", Sys.Date(), "-top10.RDS"))
