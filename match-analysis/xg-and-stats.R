pacman::p_load(understatr, tidyverse, ggthemes, ggtext, glue, ggforce, here)

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

# TODO: automate getting the following information

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
    geom_step(data = home, 
              aes(x = minute, y = cumxG), color = home_color, size = 2) +
    geom_point(data = filter(home, home$result == "Goal"), 
               aes(x = minute, y = cumxG),
               shape = 21, color = home_color, 
               stroke = 1, fill = "#043d4c", size = 4) +
    geom_step(data = away,
              aes(x = minute, y = cumxG),
              color = away_color, size = 2) +
    geom_point(data = filter(away, away$result == "Goal"),
               aes(x = minute, y = cumxG),
               shape = 21, color = away_color,
               stroke = 1, fill = "#043d4c", size = 4) +
    geom_mark_circle(data = home_goal_data,
                   aes(x = minute, y = cumxG,
                       label = paste0(player, ", ", minute, "\"")),
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
                   con.size = 0.5, con.colour = home_color) +
    geom_mark_circle(data = away_goal_data[1,],
                     aes(x = minute, y = cumxG,
                         label = paste0(player, ", ", minute, "\"")),
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
                     con.size = 0.5, con.colour = away_color) +
    geom_mark_circle(data = away_goal_data[2,],
                     aes(x = minute, y = cumxG,
                         label = paste0(player, ", ", minute, "\"")),
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
                     con.size = 0.5, con.colour = away_color) +
    scale_x_continuous(breaks = seq(
        0, ceiling(max(match_data$minute) / 5) * 5, 5)
        ) +
    # scale_y_continuous() +
    labs(
        title = glue::glue("<b style='color:{home_color};'>{home_team}: {home_goal}</b>",
                           "<span style='color:{home_color};'>  (xG: {home_xg})</span>",
                           "<br>",
                           "<b style='color:{away_color};'>{away_team}: {away_goal}</b>",
                           "<span style='color:{away_color};'>  (xG: {away_xg})</span>"),
        subtitle = glue::glue("{match_date}"),
        x = '**Time**',
        y = '**Expected Goals**',
        caption = glue::glue("<p>Data: understat.com<br>By: @rexarski</p>")
    ) +
    ggthemes::theme_solarized_2(light = FALSE, base_family = "Roboto Condensed") +
    theme(plot.title = element_markdown(face = "bold", size = 20, lineheight = 1.2),
          plot.subtitle = element_markdown(face = "bold"),
          plot.caption = element_markdown(margin = margin(t = 15)),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.title.position = 'plot',
          plot.margin = margin(25, 25, 10, 25)) +
    coord_cartesian(expand = TRUE)

xg_timeline

ggsave(plot = xg_timeline,
       here::here("match-analysis/xg-timeline-04.png"),
       height = 6, width = 8)

############# xg court #############

############# match stats #############

# get_match_stats(match_id)

