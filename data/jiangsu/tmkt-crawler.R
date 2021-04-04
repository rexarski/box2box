library(tidyverse)
library(rvest)
library(xml2)

# historical league placements
html <- read_html("https://www.transfermarkt.com/jiangsu-guoxin-sainty/platzierungen/verein/22219")
placements <- html %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)
placements <- placements[[2]]
str(placements)
colnames(placements) <- c(
  "Season", "dlt", "League", "Level",
  "W", "D", "L", "Goals", "+/-", "Points",
  "Rank", "Manager", "dlt2"
)
placements <- placements %>%
  select(-dlt, -dlt2) %>%
  as_tibble() %>%
  separate(Goals, sep = ":", into = c("G", "GA"))
write_csv(placements, "jiangsu_historical_placements.csv")

# record players
html <- read_html("https://www.transfermarkt.com/jiangsu-fc/rekordspieler/verein/22219/")
players <- html %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)
str(players)
nations <- html %>%
  html_nodes(".flaggenrahmen") %>%
  html_attr("title") %>%
  as_vector()
nations # remove China at the beginning. Set the last Italy and Brazil as duo nationalities.
players <- players[[2]]
colnames(players) <- c("index", "Player / Current club", "Nation", "Name", "Current club", "dlt", "Date of Birth", "Apps", "Goals", "Assists", "Yellow cards", "Two yellows", "Red cards")
players <- players %>%
  filter_all(any_vars(!is.na(.))) %>%
  filter(!is.na(index)) %>%
  select(-dlt, -`Player / Current club`, -Nation) %>%
  mutate(`Date of Birth` = lubridate::dmy(`Date of Birth`)) %>%
  as_tibble()
write_csv(players, "jiangsu_record_players.csv")

# top scorers
html <- read_html("https://www.transfermarkt.com/jiangsu-guoxin-sainty/topTorschuetzen/verein/22219")
scorers <- html %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)
scorers <- scorers[[2]]
colnames(scorers) <- c(
  "index", "Player / Current club", "Nation",
  "Name", "Current club", "dlt", "Date of Birth", "Apps",
  "Min/G", "Goals/Match", "Goals"
)
scorers <- scorers %>%
  filter(!is.na(index)) %>%
  select(-dlt, -`Player / Current club`, -Nation) %>%
  mutate(
    `Date of Birth` = lubridate::dmy(`Date of Birth`),
    `Min/G` = str_remove_all(`Min/G`, "[\\.']")
  ) %>%
  as_tibble()

write_csv(scorers, "jiangsu_top_scorers.csv")
