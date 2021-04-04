pacman::p_load(understatr, tidyverse, ggthemes, ggtext, glue, ggforce, here, ggsoccer, patchwork, treemapify)

############# xg timeline #############

match_data <- get_match_shots(14718)

home <- match_data %>%
  filter(h_a == "h") %>%
  add_row(minute = 0, xG = 0, .before = 1)
away <- match_data %>%
  filter(h_a == "a") %>%
  add_row(minute = 0, xG = 0, .before = 1)

if (max(match_data$minute) < 90) {
  home <- home %>% add_row(minute = 90, xG = 0)
  away <- away %>% add_row(minute = 90, xG = 0)
} else {
  home <- home %>% add_row(minute = max(match_data$minute), xG = 0)
  away <- away %>% add_row(minute = max(match_data$minute), xG = 0)
}

home <- home %>% mutate(cumxG = cumsum(xG))
away <- away %>% mutate(cumxG = cumsum(xG))

# TODO: automate getting the following information (from home and away)

home_color <- "#FFFFFF"
away_color <- "#FFCD00"
home_team <- "Fulham"
away_team <- "Leeds"
home_goal <- 1
away_goal <- 2
home_xg <- 1.47
away_xg <- 1.41
match_date <- "Mar 19 2021"

home_goal_data <- home %>% filter(result %in% c("Goal", "OwnGoal"))
away_goal_data <- away %>% filter(result %in% c("Goal", "OwnGoal"))
home_goal_data
away_goal_data

# TODO: automate adding annotations to every single goal(s)

xg_timeline <- ggplot() +
  geom_step(
    data = home,
    aes(x = minute, y = cumxG), color = home_color, size = 2
  ) +
  geom_point(
    data = filter(home, home$result == "Goal"),
    aes(x = minute, y = cumxG),
    shape = 21, color = home_color,
    stroke = 1, fill = "#043d4c", size = 4
  ) +
  geom_step(
    data = away,
    aes(x = minute, y = cumxG),
    color = away_color, size = 2
  ) +
  geom_point(
    data = filter(away, away$result == "Goal"),
    aes(x = minute, y = cumxG),
    shape = 21, color = away_color,
    stroke = 1, fill = "#043d4c", size = 4
  ) +
  geom_mark_circle(
    data = home_goal_data,
    aes(
      x = minute, y = cumxG,
      label = paste0(player, ", ", minute, "\"")
    ),
    show.legend = FALSE,
    expand = unit(.1, "mm"),
    radius = unit(0, "mm"),
    label.fontsize = 10,
    label.family = "Montserrat", label.fontface = "bold",
    label.fill = "#043d4c", label.colour = home_color,
    label.buffer = unit(2, "mm"),
    label.hjust = .25,
    con.cap = unit(0, "mm"), con.border = "all",
    con.type = "straight",
    con.size = 0.5, con.colour = home_color
  ) +
  geom_mark_circle(
    data = away_goal_data[1, ],
    aes(
      x = minute, y = cumxG,
      label = paste0(player, ", ", minute, "\"")
    ),
    show.legend = FALSE,
    expand = unit(.1, "mm"),
    radius = unit(0, "mm"),
    label.fontsize = 10,
    label.family = "Montserrat", label.fontface = "bold",
    label.fill = "#043d4c", label.colour = away_color,
    label.buffer = unit(2, "mm"),
    label.hjust = .25,
    con.cap = unit(0, "mm"), con.border = "all",
    con.type = "straight",
    con.size = 0.5, con.colour = away_color
  ) +
  geom_mark_circle(
    data = away_goal_data[2, ],
    aes(
      x = minute, y = cumxG,
      label = paste0(player, ", ", minute, "\"")
    ),
    show.legend = FALSE,
    expand = unit(.1, "mm"),
    radius = unit(0, "mm"),
    label.fontsize = 10,
    label.family = "Montserrat", label.fontface = "bold",
    label.fill = "#043d4c", label.colour = away_color,
    label.buffer = unit(2, "mm"),
    label.hjust = .25,
    con.cap = unit(0, "mm"), con.border = "all",
    con.type = "straight",
    con.size = 0.5, con.colour = away_color
  ) +
  scale_x_continuous(breaks = seq(
    0, ceiling(max(match_data$minute) / 5) * 5, 5
  )) +
  scale_y_continuous() +
  labs(
    title = glue::glue(
      "<b style='color:{home_color};'>{home_team}: {home_goal}</b>",
      "<span style='color:{home_color};'>  (xG: {home_xg})</span>",
      "<br>",
      "<b style='color:{away_color};'>{away_team}: {away_goal}</b>",
      "<span style='color:{away_color};'>  (xG: {away_xg})</span>"
    ),
    subtitle = glue::glue("{match_date}"),
    x = "**Time**",
    y = "**Expected Goals**"
    # caption = glue::glue("<p>Data: understat.com<br>By: @rexarski</p>")
  ) +
  ggthemes::theme_solarized_2(light = FALSE, base_family = "Roboto Condensed") +
  theme(
    plot.title = element_markdown(face = "bold", size = 20, lineheight = 1.2),
    plot.subtitle = element_markdown(face = "bold"),
    plot.caption = element_markdown(margin = margin(t = 15)),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    plot.title.position = "plot",
    plot.margin = margin(25, 25, 10, 25)
  ) +
  coord_cartesian(expand = TRUE)

xg_timeline

ggsave(
  plot = xg_timeline,
  here::here("match-analysis/xg-timeline.png"),
  height = 6, width = 10
)

############# xg court #############

home_goal_shots <- home %>% filter(result %in% c("Goal", "OwnGoal"))
home_ngoal_shots <- home %>% filter(!(result %in% c("Goal", "OwnGoal")))


xg_court <- ggplot() +
  annotate_pitch(fill = "#00303e", colour = "white", dimensions = pitch_opta) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#00303e")) +
  geom_point(
    data = home, aes(x = 100 * X, y = 100 * Y), shape = 16, size = 10 * sqrt(home$xG),
    color = if_else(
      home$result %in% c("Goal", "OwnGoal"),
      "royalblue", "azure"
    ), alpha = .9
  ) +
  geom_point(
    data = away, aes(x = 100 - 100 * X, y = 100 - 100 * Y), shape = 16, size = 10 * sqrt(away$xG),
    color = if_else(
      away$result %in% c("Goal", "OwnGoal"),
      "royalblue", "azure"
    ), alpha = .9
  ) +
  labs(
    title = glue::glue("<b style='color: white;'>Shots on field</b>")
    # caption = glue::glue("<p>Data: understat.com<br>By: @rexarski</p>")
  ) +
  ggthemes::theme_solarized_2(light = FALSE, base_family = "Roboto Condensed") +
  theme(
    plot.title = element_markdown(face = "bold", size = 20, lineheight = 1.2),
    plot.subtitle = element_markdown(face = "bold"),
    plot.caption = element_markdown(margin = margin(t = 15)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title.position = "plot",
    plot.margin = margin(15, 15, 10, 15)
  ) +
  coord_cartesian(expand = FALSE) +
  geom_richtext(aes(
    x = 75, y = 20, label = glue::glue("{home_team} xG: {home_xg}"), fontface = "bold",
    family = "Roboto Condensed"
  ),
  fill = NA, label.color = NA, # remove background and outline
  label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
  color = home_color, size = 12
  ) +
  geom_richtext(aes(
    x = 25, y = 20, label = glue::glue("{away_team} xG: {away_xg}"), fontface = "bold",
    family = "Roboto Condensed"
  ),
  fill = NA, label.color = NA, # remove background and outline
  label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
  color = away_color, size = 12
  )
xg_court

ggsave(
  plot = xg_court,
  here::here("match-analysis/xg-court.png"),
  height = 7.5, width = 10
)

############# match stats #############

# unfortunately understatr::match_stats() does not provide overall team data

xg_data <- match_data %>%
  group_by(h_a, player) %>%
  summarize(xg_contribution = sum(xG)) %>%
  drop_na()

xg_contr <- ggplot(xg_data, aes(
  area = xg_contribution, fill = h_a, subgroup = h_a %>% map_chr(
    function(x) {
      if_else(
        x == "a",
        away_team,
        home_team
      )
    }
  ),
  label = glue::glue(
    "{player}",
    "\n",
    "{round(xg_contribution, 4)}"
  )
)) +
  geom_treemap() +
  scale_colour_manual(
    values = c("h" = home_color, "a" = away_color),
    aesthetics = c("colour", "fill")
  ) +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(
    place = "centre", grow = T, alpha = 0.3, colour =
      "grey75", fontface = "italic", min.size = 0
  ) +
  geom_treemap_text(
    family = "Montserrat", fontface = "bold.italic", colour = "#00303e", place = "center",
    grow = TRUE, reflow = TRUE, min.size = 0, padding.x = grid::unit(5, "mm"), padding.y = grid::unit(5, "mm")
  ) +
    labs(
        title = glue::glue("<b style='color: white;'>Players' xG contribution</b>"),
        caption = glue::glue("<p>Data: understat.com<br>By: @rexarski</p>")
    ) +
    ggthemes::theme_solarized_2(light = FALSE, base_family = "Roboto Condensed") +
    theme(
        plot.title = element_markdown(face = "bold", size = 20, lineheight = 1.2),
        plot.subtitle = element_markdown(face = "bold"),
        plot.caption = element_markdown(margin = margin(t = 15)),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.title.position = "plot",
        plot.margin = margin(25, 25, 10, 25),
        legend.position = "none"
    ) +
    coord_cartesian(expand = TRUE)

xg_contr

ggsave(
    plot = xg_contr,
    here::here("match-analysis/xg-contr.png"),
    height = 6, width = 10
)

############# patchwork #############

pp <- xg_timeline / xg_court / xg_contr
pp
ggsave(
    plot = pp,
    here::here("match-analysis/xg-complete.png"),
    height = 19.5, width = 10
)