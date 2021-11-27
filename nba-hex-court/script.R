if (!require("pacman")) install.packages("pacman")
# devtools::install_github("abresler/nbastatR")
pacman::p_load(tidyverse, nbastatR, extrafont, hexbin,
               prismatic, teamcolors, cowplot)

# Sys.setenv("VROOM_CONNECTION_SIZE"=500072)

# custom theme
custom_theme <- function () { 
    theme_minimal(base_size=11, base_family="Roboto Condensed") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Get NBA teams and their names
tms <- nba_teams()
tms <- tms %>% 
    filter(isNonNBATeam == 0) %>% 
    select(nameTeam, slugTeam)

# Get NBA team colors
tm.colors <- teamcolors
tm.colors <- tm.colors %>% 
    filter(league == "nba") %>% 
    select(name, primary) %>% 
    mutate(primary = case_when(
        name == "Golden State Warriors" ~ "#1D428A",
        name == "Indiana Pacers" ~ "#002D62",
        name == "Los Angeles Lakers" ~ "#552583",
        name == "San Antonio Spurs" ~ "#000000",
        name == "Oklahoma City Thunder" ~ "#EF3B24",
        name == "Charlotte Hornets" ~ "#00788C",
        name == "Utah Jazz" ~ "#00471B",
        name == "New Orleans Pelicans" ~ "#0C2340",
        TRUE ~ primary
    )) 

devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")

df <- teams_shots(all_active_teams = T, season_types = "Regular Season", seasons = 2022)

# find out which team is on offense/defense
df <- left_join(df, tms)

# if slugTeam is the home team, then the defense must be the away team (visa versa)
df <- df %>% 
    mutate(defense = case_when(
        slugTeam == slugTeamHome ~ slugTeamAway,
        TRUE ~ slugTeamHome
    ))

# get the full name of the defensive team 
df <- left_join(df, tms, by = c("defense" = "slugTeam"))

# rename to distinugish between offensive and defensive team
df <- df %>% 
    rename("nameTeamOffense" = "nameTeam.x", 
           "nameTeamDefense" = "nameTeam.y")

# transform the location to fit the dimensions of the court, rename variables 
df <- df %>% 
    mutate(locationX = as.numeric(as.character(locationX)) / 10,
           locationY = as.numeric(as.character(locationY)) / 10 + hoop_center_y) %>% 
    rename("loc_x" = "locationX", 
           "loc_y" = "locationY")

# flip values along the y-axis
df$loc_x <- df$loc_x * -1 

# fix the Clippers name
df <- df %>% 
    mutate(nameTeamOffense = case_when(
        nameTeamOffense == "LA Clippers" ~ "Los Angeles Clippers", 
        TRUE ~ nameTeamOffense
    ))

df <- df %>% 
    mutate(slugTeam = case_when(
        nameTeamOffense == "Los Angeles Clippers" ~ "LAC", 
        TRUE ~ slugTeam
    ))

# Filter out backcourt shots or anything more than 35 feet
df <- df %>% 
    filter(zoneBasic != "Backcourt" & distanceShot <= 35)

# Create a function that helps create our custom hexs
hex_bounds <- function(x, binwidth) {
    c(
        plyr::round_any(min(x), binwidth, floor) - 1e-6,
        plyr::round_any(max(x), binwidth, ceiling) + 1e-6
    )
}

# Set the size of the hex
binwidths <- 3.5

# Calculate the area of the court that we're going to divide into hexagons
xbnds <- hex_bounds(df$loc_x, binwidths)
xbins <- diff(xbnds) / binwidths
ybnds <- hex_bounds(df$loc_y, binwidths)
ybins <- diff(ybnds) / binwidths

# Create a hexbin based on the dimensions of our court
hb <- hexbin(
    x = df$loc_x,
    y = df$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
)

# map our hexbins onto our dataframe of shot attempts
df <- mutate(df, hexbin_id = hb@cID) 

# find the leauge avg % of shots coming from each hex
la <- df %>%
    group_by(hexbin_id) %>%
    summarize(hex_attempts = n()) %>% 
    ungroup() %>% 
    mutate(hex_pct = hex_attempts / sum(hex_attempts, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename("league_average" = "hex_pct") %>% 
    select(-hex_attempts)

# Calculate the % of shots coming from each hex for each team
hexbin_stats <- df %>%
    group_by(hexbin_id, nameTeamOffense) %>%
    summarize(hex_attempts = n()) %>% 
    ungroup() %>% 
    group_by(nameTeamOffense) %>% 
    mutate(hex_pct = hex_attempts / sum(hex_attempts, na.rm = TRUE)) %>% 
    ungroup() 

hexbin_stats <- hexbin_stats %>%
    left_join(., la) %>% 
    group_by(hexbin_id) %>%
    mutate(sd_hex_pct = sd(hex_pct, na.rm = TRUE), 
           z_score = (hex_pct - league_average) / sd_hex_pct) 

# Full disclosure, no idea what this next part does
# from hexbin package, see: https://github.com/edzer/hexbin
sx <- hb@xbins / diff(hb@xbnds)
sy <- (hb@xbins * hb@shape) / diff(hb@ybnds)
dx <- 1 / (2 * sx)
dy <- 1 / (2 * sqrt(3) * sy)
origin_coords <- hexcoords(dx, dy)

hex_centers <- hcell2xy(hb)

hexbin_coords <- bind_rows(lapply(1:hb@ncells, function(i) {
    data.frame(
        x = origin_coords$x + hex_centers$x[i],
        y = origin_coords$y + hex_centers$y[i],
        center_x = hex_centers$x[i],
        center_y = hex_centers$y[i],
        hexbin_id = hb@cell[i]
    )
}))

# Merge out hexbin coordinates with our hexbin stats
hex_data <- inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")

# Adjusts the size of the hexagons
hex_data <- hex_data %>% 
    mutate( radius_factor = .99,
            adj_x = center_x + radius_factor * (x - center_x),
            adj_y = center_y + radius_factor * (y - center_y))

# merge with the tms data so that we can have the abbreviated name
hex_data <- left_join(hex_data, tms, by = c('nameTeamOffense' = 'nameTeam'))

# merge with the team colors 
hex_data <- left_join(hex_data, tm.colors, by = c("nameTeamOffense" = "name"))

# minimum of 10 attempts and have a z-score >= 0
hex_data <- hex_data %>% filter(hex_attempts >= 10 & hex_data$z_score > 0 & !is.na(slugTeam)) 

ggplot() +
    geom_polygon(data = hex_data, 
                 aes(x = adj_x, y = adj_y, alpha = sqrt(z_score), fill = primary, color = after_scale(clr_darken(fill, 0.3)), group = hexbin_id), 
                 size = .25) +
    custom_theme() +
    facet_wrap(~slugTeam, nrow = 5, strip.position = 'top') +
    scale_alpha_continuous(range = c(.05, 1)) + 
    scale_fill_identity() 

ggplot() +
    geom_polygon(data = hex_data, 
                 aes(x = adj_x, y = adj_y, alpha = sqrt(z_score), fill = primary, color = after_scale(clr_darken(fill, 0.3)), group = hexbin_id), 
                 size = .25) +
    custom_theme() +
    facet_wrap(~slugTeam, nrow = 5, strip.position = 'top') +
    scale_alpha_continuous(range = c(.05, 1)) + 
    scale_fill_identity() +
    theme(legend.position = 'none',
          line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          panel.spacing  = unit(-.25, "lines"), 
          plot.title = element_text(face = 'bold', hjust= .5, size = 15, color = 'black',  family = "Roboto Condensed"),
          plot.caption  = element_text(size = 6, hjust= .5, color = 'black', family = "Roboto Condensed"),
          strip.text = element_text(size = 8, vjust = -1, face = 'bold', family = "Roboto Condensed")) + 
    scale_y_continuous(limits = c(-2.5, 42)) +
    scale_x_continuous(limits = c(-30, 30))  +
    coord_fixed(clip = 'off') +
    labs(title =  "Where Teams Like To Shoot From\nRelative To League Average", 
         caption = "Darker and denser areas indicate a team takes more shots from that spot relative to the league as a whole")

# draw the court itself and add a floral white background to everything

p <- ggplot() +
    geom_polygon(data = hex_data, 
                 aes(x = adj_x, y = adj_y, alpha = sqrt(z_score), fill = primary, color = after_scale(clr_darken(fill, 0.3)), group = hexbin_id), 
                 size = .25) +
    custom_theme() +
    facet_wrap(~slugTeam, nrow = 5, strip.position = 'top') +
    scale_alpha_continuous(range = c(.05, 1)) + 
    scale_fill_identity() +
    theme(legend.position = 'none',
          line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          panel.spacing  = unit(-.25, "lines"), 
          plot.title = element_text(face = 'bold', hjust= .5, size = 15, color = 'black',  family = "Roboto Condensed"),
          plot.caption  = element_text(size = 6, hjust= .5, color = 'black', family = "Roboto Condensed"),
          strip.text = element_text(size = 8, vjust = -1, face = 'bold', family = "Roboto Condensed")) + 
    scale_y_continuous(limits = c(-2.5, 42)) +
    scale_x_continuous(limits = c(-30, 30))  +
    coord_fixed(clip = 'off') +
    # labs(title =  "Where Teams Like To Shoot From\nRelative To League Average", 
         # caption = "Darker and denser areas indicate a team takes more shots from that spot relative to the league as a whole")  +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc, linetype = dash),
              color = "black", size = .25) 

p <- ggdraw(p) + 
    theme(plot.background = element_rect(fill="floralwhite", color = NA))
p
ggsave("nba-hex-court/HexChart.png", p, width = 6, height = 6, dpi = 300, type = 'cairo')
ggsave("nba-hex-court/HexChart.svg", p, width = 6, height = 6, dpi = 300)
