rm(list = ls())
gc()

pacman::p_load(devtools, tidyverse, ggsoccer, viridis, patchwork)

devtools::install_github("statsbomb/StatsBombR", force = TRUE)
library(StatsBombR)

CompAnalyze <- FreeCompetitions() %>%
  filter(competition_id == 11, season_name == "2017/2018")
Matches <- FreeMatches(CompAnalyze)
MatchEventData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = F) # sadly parallel is only available on Windows, reading line by line is painful.

passdata <- MatchEventData %>%
  filter(type.name == "Pass", team.name == "Barcelona")

unique(passdata$player.name)

pass_heatmap <- function(player) {
  bin <- 4
  x_bin <- 120 / bin
  y_bin <- 80 / bin
  passfx <- seq(0, 120, by = bin)
  passfy <- seq(0, 80, by = bin)

  PassFlow <- tibble(x = numeric(), y = numeric(), endX = numeric(), endY = numeric(), countP = numeric())
  PlayerPF <- passdata %>% filter(player.name == player)

  PlayerPF_lite <- PlayerPF %>%
    select(location, pass.end_location) %>%
    unnest_wider(location, names_sep = ".") %>%
    unnest_wider(pass.end_location, names_sep = ".") %>%
    rename(
      location.x = location.1,
      location.y = location.2,
      pass.end_location.x = pass.end_location.1,
      pass.end_location.y = pass.end_location.2
    )

  for (i in 1:x_bin) {
    filterx <- PlayerPF_lite %>%
      filter(location.x >= passfx[i]) %>%
      filter(location.x < passfx[i + 1])

    for (j in 1:y_bin) {
      minY <- passfy[j]
      maxY <- passfy[j + 1]

      filtery <- filterx %>%
        filter(location.y >= minY) %>%
        filter(location.y < maxY)

      if (nrow(filtery) >= 1) {
        me_x <- mean(filtery$location.x)
        me_y <- mean(filtery$location.y)
        me_ex <- mean(filtery$pass.end_location.x)
        me_ey <- mean(filtery$pass.end_location.y)

        count <- nrow(filtery)

        PassFlow <- PassFlow %>%
          add_row(
            x = me_x,
            y = me_y,
            endX = me_ex,
            endY = me_ey,
            countP = count
          )
      }
    }
  }

  PassFlow

  write.csv(PassFlow, paste0("pass-flow-graph/data/", str_replace(player, " ", "-"), ".csv"), row.names = FALSE)

  p <- ggplot(data = PassFlow) +
    annotate_pitch(fill = "grey20", colour = "grey90", dimensions = pitch_statsbomb) +
    theme_pitch() +
    theme(panel.background = element_rect(fill = "grey20")) +
    geom_bin2d(
      data = PlayerPF_lite, aes(x = location.x, y = location.y), alpha = .6,
      binwidth = c(bin, bin), position = "identity"
    ) +
    scale_fill_viridis_c() +
    # scale_fill_fermenter(n.breaks = 9, palette = "PuOr") +
    geom_segment(aes(x = x, y = y, xend = endX, yend = endY, alpha = countP),
      color = "white", lineend = "round", size = 1, arrow = arrow(length = unit(1.5, "mm"))
    ) +
    scale_y_reverse() +
    labs(
      title = player
    ) +
    theme(legend.position = "none")
  p

  ggsave(paste0("pass-flow-graph/img/", str_replace(player, " ", "-"), ".png"), width = 12, height = 8)
  return(p)
}

pass_heatmap("Ivan Rakitić")
pass_heatmap("Marc-André ter Stegen")
pass_heatmap("Lionel Andrés Messi Cuccittini")
# (pass_heatmap("Ivan Rakitić") | pass_heatmap("Gerard Piqué Bernabéu") |
#         pass_heatmap("Jordi Alba Ramos")) /
#     (pass_heatmap("Marc-André ter Stegen") | pass_heatmap("Sergi Roberto Carnicer") |
#          pass_heatmap("Lionel Andrés Messi Cuccittini"))
#
