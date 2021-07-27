

# Info about flight distance, time, direction, coordinates
travel1819 <- nba_travel(start_season = 2019,
                         end_season = 2019,
                         return_home = 3,
                         phase = "RS") #airball

# Data about back to back, first or second leg etc. 
density1819 <- nba_density(travel1819) #airball

#this data has tipoff time
schedule1819 <- get_schedule(2018) %>%
  filter(season_type == "REG") #NBAr
#main useful variable from schedule1617 is game_time_utc. Havent actually merged schedule1617 with anything yet


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density1819 <- merge(x = travel1819, y = density1819,
                                by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away"))

regseason1819 <- mutate(travel_and_density1819, Win = (travel_and_density1819$"W/L" == "W")) %>%
  select(c("Season", "Month", "Week", "Date", "Team", "Opponent", "Visitor", "City", "Win", 
           "Distance", "Route", "Rest", "TZ", "Shift (hrs)", "Flight Time", "Direction (E/W)", 
           "Latitude", "Longitude", "B2B", "B2B-1st", "B2B-2nd", "3in4", "4in5", "5in7")) %>%
  rename(b2b_1st = "B2B-2nd") %>%
  rename(b2b_2nd = "B2B-1st") %>%
  rename(direction = "Direction (E/W)") %>%
  rename(shift = "Shift (hrs)") %>%
  rename(three_in_four = "3in4") %>%
  rename(four_in_five = "4in5") %>%
  rename(five_in_seven = "5in7")

#The given flight durations are funky and have strings but I just wanna extra the time
flight_durations1819 = c()

for (d in regseason1819$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations1819 = c(flight_durations1819, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations1819 = c(flight_durations1819, hours)
  }
  else {
    flight_durations1819 = c(flight_durations1819, 0)
  }
}

regseason1819 <- mutate(regseason1819, flight_duration = flight_durations1819) %>%
  select(- "Flight Time")

team_boxscores1819 <- get_team_boxscore(2018) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus)) %>%
  mutate(Team = case_when(
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    TRUE ~ Team)
  )


regseason1819 <- merge(x = regseason1819, y = team_boxscores1819,
                       by = c("Date", "Team")) %>%
  rename(score_diff = plus_minus) %>%
  mutate(traveling_west = (direction == "West")) %>% 
  mutate(traveling_east = (direction == "East")) %>%
  mutate(B2B = (B2B == "Yes")) %>%
  mutate(b2b_1st = (b2b_1st == "Yes")) %>%
  mutate(b2b_2nd = (b2b_2nd == "Yes")) %>%
  mutate(three_in_four = (three_in_four == "Yes")) %>%
  mutate(four_in_five = (four_in_five == "Yes")) %>%
  mutate(five_in_seven = (five_in_seven == "Yes"))

avg_score_diffs_home1819 <- regseason1819 %>%
  filter(Visitor == FALSE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_home = mean))

avg_score_diffs_away1819 <- regseason1819 %>%
  filter(Visitor == TRUE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_away = mean))

regseason1819 <- merge(x = regseason1819, y = avg_score_diffs_home1819, 
                       by = "Team")

regseason1819 <- merge(x = regseason1819, y = avg_score_diffs_away1819, 
                       by = "Team") %>%
  arrange(Date)


#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs1819 = c()

for (i in c(1:nrow(regseason1819))) {
  if (regseason1819$Visitor[i] == TRUE) { #if they are visitor
    adj_score_diffs1819 = c(adj_score_diffs1819, 
                        regseason1819$score_diff[i] - regseason1819$avg_score_diff_away[i])
  }
  else { #if they are home
    adj_score_diffs1819 = c(adj_score_diffs1819, 
                        regseason1819$score_diff[i] - regseason1819$avg_score_diff_home[i])
  }
}

regseason1819 <- regseason1819 %>%
  mutate(adjusted_score_diff = adj_score_diffs1819)

dtd_records1819 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/record_by_day_2018_19_season.csv")

dtd_records1819$Team <- stringr::str_replace(dtd_records1819$team, '\\*', '') 

dtd_records1819 <- select(dtd_records1819, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

regseason1819 <- merge(x= regseason1819, y = dtd_records1819,
                       by = c("Date", "Team"))

opp_win_records1819 <- c()
for (i in c(1:nrow(regseason1819))) {
  opp <- regseason1819$Opponent[i]
  day <- regseason1819$Date[i]
  opp_record <- filter(dtd_records1819, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records1819 <- c(opp_win_records1819, opp_record)
}

regseason1819 <- regseason1819 %>%
  mutate(opp_win_percent = opp_win_records1819)

regseason1819 <- regseason1819 %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

write_csv(regseason1819, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/timesaver/temp1819.csv")

regseason1819 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/timesaver/temp1819.csv")

ratings1819 <- get_general(
  season = 2018,
  type = "Team",
  measure_type = "Advanced",
  per_mode = "Totals",
  season_type = "Regular+Season",
  season_segment = "",
  game_segment = "",
  date_from = "",
  date_to = "",
  outcome = "",
  period = "0",
  opponent_team_id = "0",
  team_id = "0",
  verbose = TRUE
) %>%  
  select(c("team_name", "off_rating", "def_rating", "net_rating", "pace")) %>%
  rename(Team = team_name)

ratings1819 <- ratings1819 %>%
  mutate(Team = case_when(
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    TRUE ~ Team)
  )

regseason1819 <- merge(x = regseason1819, y = ratings1819,
                       by = "Team")

regseason1819 <- merge(x = regseason1819, y = ratings1819,
                       by.x = "Opponent", by.y = "Team")

regseason1819 <- regseason1819 %>%
  rename(off_rating = "off_rating.x") %>%
  rename(def_rating = "def_rating.x") %>%
  rename(net_rating = "net_rating.x") %>%
  rename(pace = "pace.x") %>%
  rename(opp_off_rating = "off_rating.y") %>%
  rename(opp_def_rating = "def_rating.y") %>%
  rename(opp_net_rating = "net_rating.y") %>%
  rename(opp_pace = "pace.y") 

regseason1819 <- regseason1819 %>%
  mutate(net_rating_diff = net_rating - opp_net_rating) %>%
  mutate(pace_diff = pace - opp_pace)


performance1819 <- get_general(
  season = 2018,
  type = "Team",
  measure_type = "Base",
  per_mode = "PerGame",
  season_type = "Regular+Season",
  season_segment = "",
  game_segment = "",
  date_from = "",
  date_to = "",
  outcome = "",
  period = "0",
  opponent_team_id = "0",
  team_id = "0",
  verbose = TRUE) %>%
  rename(Team = team_name)

performance1819 <- performance1819 %>%
  mutate(Team = case_when(
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    TRUE ~ Team)
  )

regseason1819 <- merge(regseason1819, performance1819,
                       by = "Team")

opp_performance1819 <- performance1819 %>%
  select(c(Team, fgm,fg_pct, fg3m, fg3_pct, ftm, ft_pct, reb, ast, tov, stl, blk))

regseason1819 <- merge(x = regseason1819, y = opp_performance1819,
                       by.x = "Opponent", by.y = "Team")

regseason1819 <- regseason1819 %>%
  rename(fgm = "fgm.x") %>%
  rename(fg_pct = "fg_pct.x") %>%
  rename(fg3m = "fg3m.x") %>%
  rename(fg3_pct = "fg3_pct.x") %>%
  rename(ftm = "ftm.x") %>%
  rename(ft_pct = "ft_pct.x") %>%
  rename(reb = "reb.x") %>%
  rename(ast = "ast.x") %>%
  rename(tov = "tov.x")  %>%
  rename(stl = "stl.x") %>%
  rename(blk = "blk.x")  %>%
  rename(opp_fgm = "fgm.y") %>%
  rename(opp_fg_pct = "fg_pct.y") %>%
  rename(opp_fg3m = "fg3m.y") %>%
  rename(opp_fg3_pct = "fg3_pct.y") %>%
  rename(opp_ftm = "ftm.y") %>%
  rename(opp_ft_pct = "ft_pct.y") %>%
  rename(opp_reb = "reb.y") %>%
  rename(opp_ast = "ast.y") %>%
  rename(opp_tov = "tov.y") %>%
  rename(opp_stl = "stl.y") %>%
  rename(opp_blk = "blk.y")

write_csv(regseason1819, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1819.csv")



