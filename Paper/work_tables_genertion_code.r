## Author: Austin Stephen 
## Date: 8/18/2021
## Purpose: Generating figures for the "Exertion and Player Prefromance" section 
#       of the paper.

library(tidyverse)
library(gt)

# Table generation --------------------------------------------------------
name <- c("points", "points",
          "fg_pct", "fg_pct", 
          "fg3_pct", "fg3_pct",
          "ft_pct", "ft_pct",
          "turnovers","turnovers",
          "off_rebound", "off_rebound",
          "def_rebound", "def_rebound",
          "steals","steals")

min_played <- rep(c(4,12),8)

n <- c(73959,64527,
       72639,64269,
       51100,47020,
       73959,64527, 
       73959,64527, 
       73959,64527, 
       73959,64527,
       73959,64527)

coeff_val <- c(0.14210, 0.61772, 
               0.0130508, 0.024494,
               0.001034, 0.0004391,
               -7.703e-05, 0.001163, 
               -0.025066, 0.032852, 
               -0.100168, -0.046087, 
               -0.122862, 0.028470, 
               0.049214, 0.135970)

p_val <- c(0.0112, 2.74e-16, 
           8.39e-09, 2e-16,
           0.795, 0.924,
           0.98585, 0.7986, 
           0.196, 0.191, 
           9.02e-08, 0.0565, 
           0.00031, 0.513, 
           0.000996,2.65e-12) 

adj_r_sq <- c( 7.376e-05 ,0.001024, 
               0.0004443, 0.001379,
               -1.828e-05, -2.11e-05,
               -2.304e-05, -2.23e-05,
               9.096e-06, 1.106e-05,
               0.000374, 4.094e-05,
               0.0001629, -8.888e-06,
               0.0001334, 0.000744)

table <- data.frame(name, min_played, n,coeff_val, p_val, adj_r_sq)
table$coeff_val <- round(table$coeff_val, 4)
table_4 <- table%>% filter(min_played == 4) %>% select(-min_played)
table_12 <- table%>% filter(min_played == 12) %>% select(-min_played)

table_4 <- table_4 %>% rename("p-value"= p_val,
                              "coeff value" = coeff_val,
                              "adjusted R^2" = adj_r_sq)

table_12 <- table_12 %>% rename("p-value"= p_val,
                                "coeff value" = coeff_val,
                                "adjusted R^2" = adj_r_sq)
library(gt)

res_4 <- table_4 %>%
  gt(rowname_col = "name") %>%
  tab_header(
    title = md("**Work and Player Perfromance**"),
    subtitle = md("Only players in the game for >4 min")
  ) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% 
  cols_align(align = "left",
             columns = c(2,4)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "grey")
    ),
    locations = cells_body(
      rows = c(2,4,6,8))
  )%>%
  tab_footnote(
    footnote = "one field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg_pct")
  ) %>%
  tab_footnote(
    footnote = "one 3-point field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg3_pct")
  )  %>%
  tab_source_note(
    source_note = "Using tracking data from 2014, 2015, and 2016 seasons"
  )


res_12 <- table_12 %>%
  gt(rowname_col = "name") %>%
  tab_header(
    title = md("**Work and Player Perfromance**"),
    subtitle = md("Only players in the game for >12 min")
  ) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% 
  cols_align(align = "left",
             columns = c(2,4)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "grey")
    ),
    locations = cells_body(
      rows = c(2,4,6,8))
  ) %>%
  tab_footnote(
    footnote = "one field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg_pct")
  ) %>%
  tab_footnote(
    footnote = "one 3-point field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg3_pct")
  ) %>%
  tab_source_note(
    source_note = "Using tracking data from 2014, 2015, and 2016 seasons"
  )

library(webshot)
gtsave(res_4,"work_and_player_perfromance_4min.png",path= "./Paper/")
gtsave(res_12,"work_and_player_perfromance_12min.png",path= "./Paper/")


# Scatter plot ------------------------------------------------------------

# Reading in the tracking data --------------------------------------------

sn_16_17   <- read_csv(
  "data/in_game_distance_data/tracking_by_player_season_daily_2016_17.csv")

sn_15_16   <- read_csv(
  "data/in_game_distance_data/tracking_by_player_season_daily_2015_16.csv")

sn_14_15   <- read_csv(
  "data/in_game_distance_data/tracking_by_player_season_daily_2014_15.csv")


# Reading in the box score data -------------------------------------------

bx_16_17   <- read_csv(
  "data/box_scores/box_scores_game_by_game_player_2016_17.csv")

bx_15_16   <- read_csv(
  "data/box_scores/box_scores_game_by_game_player_2015_16.csv")

bx_14_15   <- read_csv(
  "data/box_scores/box_scores_game_by_game_player_2014_15.csv")

season_box_scores_player_2010_20 <- read.csv(
  "data/box_scores/season_box_scores_player_2010_20.csv") %>%
  filter(season %in% c( 2014, 2015,2016)) %>%
  mutate(season = case_when(
    season == 2014 ~ "2014_15",
    season == 2015 ~ "2015_16",
    season == 2016 ~ "2016_17")) %>%
  select("player_id","season", "fgm", "fga", "fg_pct", "fg3_pct","ft_pct", 
         "oreb", "dreb", "ast","tov","stl","blk", "pts") %>%
  rename("fgm_sn" = fgm,
         "fga_sn" = fga,
         "fg_pct_sn" = fg_pct,
         "fg3_pct_sn" = fg3_pct,
         "ft_pct_sn" = ft_pct,
         "oreb_sn" = oreb,
         "dreb_sn" = dreb,
         "ast_sn" = ast,
         "tov_sn" = tov,
         "stl_sn" = stl,
         "blk_sn" = blk,
         "pts_sn" = pts
  )

# Joining the tracking data to the box score data -------------------------

## Getting the seasons game ids
schedules_2014 <- read.csv("data/schedules/season_2014_2015.csv")
schedules_2015 <- read.csv("data/schedules/season_2015_2016.csv")
schedules_2016 <- read.csv("data/schedules/season_2016_2017.csv")

schedules <- rbind(schedules_2014, schedules_2015, schedules_2016)

## de-clutter the global env
rm(schedules_2014, schedules_2015,schedules_2016)

## dropping unneeded schedule rows 
schedules <- schedules %>% select(c(idGame, dateGame))

## connecting all tracking seasons into 1 data frame
sn_14_15 <- sn_14_15 %>% mutate(season = "2014_15")
sn_15_16 <- sn_15_16 %>% mutate(season = "2015_16")
sn_16_17 <- sn_16_17 %>% mutate(season = "2016_17")

tracking <- rbind(sn_14_15, sn_15_16, sn_16_17)

## connecting all box scores into 1 data frame
bx_14_15 <- bx_14_15 %>% mutate(season = "2014_15")
bx_15_16 <- bx_15_16 %>% mutate(season = "2015_16")
bx_16_17 <- bx_16_17 %>% mutate(season = "2016_17")

box_scores <- rbind(bx_14_15, bx_15_16, bx_16_17)

## Changing date format for tracking 
tracking$date <- as.Date(tracking$date, "%m/%d/%Y")

# joining box scores with schedules on game id for date
box_scores <- merge( schedules, box_scores, by= "idGame"
)

# joining with distance on game id
gbg_player_w_distance <- merge( tracking, box_scores,
                                by.x= c("player_id","date","season"),
                                by.y= c("idPlayer","dateGame", "season")
) %>%
  select("player_id","idGame","team_id","player_name", "groupStartPosition",
         "season",w:plusminus, -c("namePlayer", "gp", "slugTeam", "idTeam", "min1"))

## joining season averages 
gbg_player_w_distance <- merge( gbg_player_w_distance,
                                season_box_scores_player_2010_20,
                                by.x = c("player_id", "season"),
                                by.y = c("player_id", "season"))

## cleanning the global env 
rm(bx_14_15, bx_15_16, bx_16_17, sn_14_15, sn_15_16, sn_16_17, schedules,
   tracking, season_box_scores_player_2010_20, box_scores)


# Adding difference from season avg cols ----------------------------------

gbg_player_w_distance <- gbg_player_w_distance %>%
  filter(!is.na(dist_miles)) %>%
  group_by(player_id) %>% mutate(
    ftpct = ifelse(fta > 0,(ftm / fta), NA),
    avg_dist_miles_sn = mean(dist_miles),
    avg_dist_miles_off_sn = mean(dist_miles_off),
    avg_dist_miles_def_sn = mean(dist_miles_def),
    avg_speed_sn = mean(avg_speed),
    avg_speed_off_sn = mean(avg_speed_off),
    avg_speed_def_sn = mean(avg_speed_def),
    diff_from_sn_avg_dist = dist_miles - avg_dist_miles_sn,
    diff_from_sn_avg_off = dist_miles_off - avg_dist_miles_off_sn,
    diff_from_sn_avg_def = dist_miles_def - avg_dist_miles_def_sn,
    diff_from_sn_avg_speed = avg_speed - avg_speed_sn,
    diff_from_sn_avg_speed_def = avg_speed_def - avg_speed_def_sn,
    diff_from_sn_avg_speed_off = avg_speed_off - avg_speed_off_sn,
    diff_from_sn_avg_fgpct = pctFG - fg_pct_sn,
    diff_from_sn_avg_fg3pct = pctFG3 - fg3_pct_sn,
    diff_from_sn_avg_oreb = oreb - oreb_sn,
    diff_from_sn_avg_dreb = dreb - dreb_sn,
    diff_from_sn_avg_stl = stl - stl_sn,
    diff_from_sn_avg_tov = tov - tov_sn,
    diff_from_sn_avg_ftpct = ftpct - ft_pct_sn,
    diff_from_sn_avg_pts = pts - pts_sn
  )


gbg_12_plus_min_filt <-  gbg_12_plus_min %>%
  filter(fga > 0 )

gbg_12_plus_min <- gbg_player_w_distance %>% filter(min >12)

gbg_12_plus_min_filt %>%
  ggplot(aes(x=diff_from_sn_avg_fgpct, y = diff_from_sn_avg_speed_off ))+
  geom_point(alpha = .025)+
  #geom_smooth(method="lm")+
  geom_abline(slope = 0.024494, intercept = -0.002753, color = "darkred",
              size = 1) +
  labs(x= "Difference From Season Mean fg %",
       y = "Difference From Season Mean Work",
       title = "Relative fg % and Time Adjusted Relative Work")+
  xlim(c(-.5,.5))+
  ylim(c(-1.25,1.25))+
  theme_minimal()+
  theme(axis.title=element_text(size=14, face = "plain"),
        title = element_text(size=16,face = "bold"))
