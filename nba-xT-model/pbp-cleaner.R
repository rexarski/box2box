gc()
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, summarytools)

# 1. load data

# from 10mb data -> much more than that
# Start over to get shot data from PBPstats, previously got hand-tracked 3-point shooting data only.

# 2020-2021 regular season shot data (free-throws excluded).


# shots_dat <- read_csv("data/nba-pbp/pbpstats-tracking-shots.csv") %>%
#     clean_names()

files <- list.files("data/nba-pbp/2020-raw-by-team", full.names = T)
data.list <- lapply(files, read_csv)
shots_dat <- do.call(bind_rows, data.list)
rm(data.list)
shots_dat <- shots_dat %>% clean_names()

team_info <- read_csv("data/nba-team-data/codes-and-colors.csv") %>%
    clean_names()

glimpse(shots_dat) # 188975 x 37

# 2. remove redundant variables

shots_dat <- shots_dat %>%
    select(!c(gameid, eventnum, oreboundedshoteventnum,
              oreboundedrebeventnum, offense_team_id, defense_team_id,
              possession_start_type, possession_start_time,
              blockplayerid, assistplayerid))

# 3. reorder remaining variables

# usually I would use relocate(), but here we need to reorder all variables entirely, so just use select()

shots_dat <- shots_dat %>%
    select(
        player, playerid, team, assist_player, lineupid, # offensive player
        opponent, block_player, opplineupid, # defensive player
        date, period, possessionnumber, time, shot_clock, # time
        margin, # game environment
        x, y, # location
        shottype, assisted, putback, closest_def_dist, touch_time,
        dribble_range, # shot-setup
        value, made, blocked, and1, # shot-result
        video_url # miscellaneous
    )

# 4. get a list of player-id for reference

players <- shots_dat %>%
    select(playerid, player) %>%
    distinct() %>%
    rename(name = player) %>%
    write_csv("data/nba-pbp/player-id.csv")

# 5. check legitimacy of these variables (remove outliers and alien NAs)

# possession_number is the sequential number of the game play, not "how many passes before the shot"

shots_dat <- shots_dat %>% select(-possessionnumber)

shots_dat %>%
    filter_at(vars(playerid, player, team, lineupid,
                   opponent, opplineupid, date,
                   period, time,
                   # shot_clock,
                   margin, x, y, shottype,
                   # assisted,
                   putback,
                   # closest_def_dist,
                   # touch_time,
                   # dribble_range,
                   value, made,
                   # blocked,
                   and1
                   # video_url
                   ), any_vars(is.na(.)))

# For now, just use summarytools::descr() to take a look at all the numeric data,
# no obviously visible issue here.
shots_dat %>% descr()

# For the rest of NAs in data, they won't affect our model building in general.

shots_dat %>% filter(time < 0 | time > 720)
shots_dat %>% filter(is.na(assisted) & made==TRUE)
shots_dat %>% filter(blocked & is.na(block_player))
shots_dat %>% filter(blocked & made)

# this record messed up the team of Tobias Harris
# https://videos.nba.com/nba/pbp/media/2021/01/25/0022000257/591/c4167627-a8e5-a879-be14-5daa0ed0dd63_960x540.mp4

shots_dat %>% filter(assisted & is.na(assist_player)) %>% View()

shots_dat <- shots_dat %>%
    mutate(assist_player = replace(assist_player, assisted==TRUE & is.na(assist_player), "Ben Simmons"),
           team = replace(team, team=="DET" & player=="Tobias Harris" & opponent=="PHI", "PHI"),
           opponent = replace(opponent, team=="PHI" & player=="Tobias Harris" & opponent=="PHI", "DET"))

# 6. create new features

## 1. break lineupid into 5 different ids, also exclude his own playerid
## 2. break opplineupid into 5 different ids

shots_dat <- shots_dat %>%
    separate(lineupid, sep="-", into=c("teammate_1", "teammate_2", "teammate_3", "teammate_4", "teammate_5")) %>%
    separate(opplineupid, sep="-", into=c("opplayer_1", "opplayer_2", "opplayer_3", "opplayer_4", "opplayer_5"))

# this is more fun only, even the result does not align with previous cleaned data, it does not matter that much

# 7. save cleaned data
write_csv(shots_dat, "data/nba-pbp/pbpstats-tracking-shots-cleaned.csv")

# 8. split and save data

# split and save by team
shots_dat %>%
    group_by(team) %>%
    group_walk(~ write_csv(.x, paste0("data/nba-pbp/2020-cleaned-by-team/team-shots-",
                                      .y$team,
                                      ".csv")))
# split and save by opponent
shots_dat %>%
    group_by(opponent) %>%
    group_walk(~ write_csv(.x, paste0("data/nba-pbp/2020-cleaned-by-opponent/opponent-shots-",
                                      .y$opponent,
                                      ".csv")))
