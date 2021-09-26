gc()
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, extrafont, glue, corrplot, tm)

shots_dat <- read_csv("data/nba-pbp/pbpstats-tracking-shots-cleaned.csv")

coordinates <- ggplot(shots_dat, aes(x=x, y=y)) +
    geom_point(alpha = 0.05, size = .25, color = "#112987") +
    ylim(0, 400) +
    coord_equal() +
    labs(
        title = "Coordinates of shooting attempts",
        subtitle = "2020-2021 NBA regular season",
        caption = glue("By: Rui Qiu (rq47)"),
        x = "x-coordinate",
        y = "y-coordinate"
    ) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    )
ggsave(
    plot = coordinates, "nba-xT-model/04-coordinates.png",
    height = 9, width = 11, dpi = 100
)

shots_sec <- shots_dat %>%
    mutate(seconds = case_when(
        period <= 4 ~ ((period-1)*720 + (720 - time)),
        period > 4 ~ (4*720 + (period-5)*300 + (300 - time))
    )) %>%
    select(seconds)

shot_time <- ggplot(shots_sec, aes(x=seconds)) +
    geom_histogram(binwidth = 24, fill="#112987") +
    labs(
        title = "Time-based distribution of shooting attempts",
        subtitle = "2020-2021 NBA regular season",
        caption = glue("By: Rui Qiu (rq47)"),
        x = "Time lapse (in second)",
        y = "count"
    ) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    )
ggsave(
    plot = shot_time, "nba-xT-model/04-shot-time.png",
    height = 9, width = 11, dpi = 100
)

rm(coordinates)
rm(shot_time)
rm(shots_sec)

team_color <- read_csv("data/nba-team-data/codes-and-colors.csv")
was <- read_csv("data/nba-pbp/2020-cleaned-by-team/team-shots-WAS.csv")
was_colors <- team_color %>%
    filter(team_code=="WAS") %>%
    select(color1, color2, color3)

was_shots <- ggplot(was, aes(x=x, y=y)) +
    geom_point(alpha = .2, size = .8, color=was_colors$color1) +
    ylim(0, 400) +
    coord_equal() +
    labs(
        title = "Shooting attempts of Washington Wizards",
        subtitle = "2020-2021 NBA regular season",
        caption = glue("By: Rui Qiu (rq47)"),
        x = "x-coordinate",
        y = "y-coordinate"
    ) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    )
ggsave(
    plot = was_shots, "nba-xT-model/04-was-shots.png",
    height = 9, width = 11, dpi = 100
)

rm(list=ls())

reddit_text <- read_csv("data/nba-reddit/2021-09-13-tidy-text.csv")
reddit_text <- reddit_text %>%
    top_n(20)
top_words <- ggplot(reddit_text, aes(x=reorder(word, n), y=n)) +
    geom_col(fill = "#112987") +
    xlab(NULL) +
    coord_flip() +
    labs(
        title = "Top words appeared top 10 posts in /r/nba",
        subtitle = "Date: September 13, 2021",
        caption = glue("By: Rui Qiu (rq47)"),
        x = "Counts",
        y = "Words"
    ) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    )
ggsave(
    plot = top_words, "reddit-nba-scraper/04-top-words.png",
    height = 9, width = 11, dpi = 100
)

tweets <- read_csv("data/nba-tweets/tweets-corpus-cleaned.csv")
tweets_with_url <- ggplot(tweets, aes(x=contains_url,
                                      fill=contains_url)) +
    geom_bar() +
    xlab(NULL) +
    labs(
        title = "Tweets by NBA players",
        subtitle = "Does it contain a URL?",
        caption = glue("By: Rui Qiu (rq47)"),
        y = "Counts"
    ) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        legend.position = "None"
    ) +
    scale_fill_manual(values=c("TRUE" = "#f8b501",
                               "FALSE" = "#112987"))
tweets_with_url
ggsave(
    plot = tweets_with_url, "nba-player-tweet-scraper/04-tweets-with-urls.png",
    height = 9, width = 11, dpi = 100
)

news <- read_csv("data/nba-news-source/news-2021-09-25.csv")
news <- news %>%
    select(-Source)
corp <- Corpus(VectorSource(rbind(news$Headline)))
corp <- tm_map(corp, removeWords, stopwords("english"))
minFreq <- 8
DocsTermsMat <- DocumentTermMatrix(corp, control = list(removePunctuation = FALSE, bounds = list(global = c(minFreq,Inf))))
dtm <- as.matrix(DocsTermsMat)
corr <- cor(dtm)
corrplot(corr, type = "upper")
