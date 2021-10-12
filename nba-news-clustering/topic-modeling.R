if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, glue,
               topicmodels, tidytext, quanteda)

set.seed(2021)

espn <- read_csv("./data/nba-news-source/espn.com-2021-10-09.csv")
macrumors <- read_csv("./data/nba-news-source/macrumors.com-2021-10-09.csv")
nasa <- read_csv("./data/nba-news-source/nasa.gov-2021-10-09.csv")
polygon <- read_csv("./data/nba-news-source/polygon.com-2021-10-09.csv")

df <- espn %>%
    bind_rows(macrumors, nasa, polygon) %>%
    unite(Text, c(Title, Headline), sep = " ")

dfm <- df$Text %>%
    quanteda::tokens(remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE) %>%
    quanteda::tokens_select(pattern = stopwords("en"), selection = "remove") %>%
    quanteda::dfm(verbose = FALSE)

td <- tidy(dfm)
# td <- td %>%
#     mutate(source=as.integer(str_extract(document, "\\d+"))) %>%
#     mutate(source=case_when(
#         source <= 100 ~ "ESPN",
#         (source <= 200 & source > 100) ~ "MacRumors",
#         (source <= 300 & source > 200) ~ "NASA",
#         (source <= 400 & source > 300) ~ "Polygon"
#     )) %>%
#     select(-document) %>%
#     relocate(source) %>%
#     rename(document = source)
td

tf_idf <- td %>%
    bind_tf_idf(term, document, count) %>%
    arrange(desc(tf_idf))
tf_idf

tf_idf2 <- tf_idf %>%
    mutate(source=as.integer(str_extract(document, "\\d+"))) %>%
        mutate(source=case_when(
            source <= 100 ~ "ESPN",
            (source <= 200 & source > 100) ~ "MacRumors",
            (source <= 300 & source > 200) ~ "NASA",
            (source <= 400 & source > 300) ~ "Polygon"
        ))
tf_idf2    

dtm <- dfm %>%
    convert(to="topicmodels")

news_LDA <- LDA(dtm, k = 4, control = list(seed = 2021))
news_LDA

news_topics_beta <- tidy(news_LDA, matrix = "beta")
news_topics

news_top_terms <- news_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    arrange(topic, -beta)

gtopics <- news_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        title = glue("Top 10 terms by topic"),
        subtitle = "Ranked by beta value",
        caption = glue(
            "Rui Qiu (rq47)"
        ))

ggsave(gtopics, filename = glue("./nba-news-clustering/topic-modeling-terms.png"), device = "png", height = 9, width = 11, dpi = 100)

