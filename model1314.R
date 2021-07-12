

# Info about flight distance, time, direction, coordinates
travel1314 <- nba_travel(start_season = 2014,
                         end_season = 2014,
                         return_home = 3,
                         phase = "RS") #airball

# Data about back to back, first or second leg etc. 
density1314 <- nba_density(travel1314) #airball


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density1314 <- merge(x = travel1314, y = density1314,
                                by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away")) %>%
  filter(Rest != 15)

regseason1314 <- mutate(travel_and_density1314, Win = (travel_and_density1314$"W/L" == "W")) %>%
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
flight_durations1314 = c()

for (d in regseason1314$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations1314 = c(flight_durations1314, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations1314 = c(flight_durations1314, hours)
  }
  else {
    flight_durations1314 = c(flight_durations1314, 0)
  }
}

regseason1314 <- mutate(regseason1314, flight_duration = flight_durations1314) %>%
  select(- "Flight Time")

team_boxscores1314 <- get_team_boxscore(2013) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus)) 


regseason1314 <- merge(x = regseason1314, y = team_boxscores1314,
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

avg_score_diffs_home1314 <- regseason1314 %>%
  filter(Visitor == FALSE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_home = mean))

avg_score_diffs_away1314 <- regseason1314 %>%
  filter(Visitor == TRUE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_away = mean))

regseason1314 <- merge(x = regseason1314, y = avg_score_diffs_home1314, 
                       by = "Team")

regseason1314 <- merge(x = regseason1314, y = avg_score_diffs_away1314, 
                       by = "Team") %>%
  arrange(Date)


#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs1314 = c()

for (i in c(1:nrow(regseason1314))) {
  if (regseason1314$Visitor[i] == TRUE) { #if they are visitor
    adj_score_diffs1314 = c(adj_score_diffs1314, 
                            regseason1314$score_diff[i] - regseason1314$avg_score_diff_away[i])
  }
  else { #if they are home
    adj_score_diffs1314 = c(adj_score_diffs1314, 
                            regseason1314$score_diff[i] - regseason1314$avg_score_diff_home[i])
  }
}

regseason1314 <- regseason1314 %>%
  mutate(adjusted_score_diff = adj_score_diffs1314)

dtd_records1314 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/record_by_day_2013_14_season.csv")

dtd_records1314$Team <- stringr::str_replace(dtd_records1314$team, '\\*', '') 

dtd_records1314 <- select(dtd_records1314, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

regseason1314 <- merge(x= regseason1314, y = dtd_records1314,
                       by = c("Date", "Team")) %>%
  filter(Rest <= 3)

opp_win_records1314 <- c()
for (i in c(1:nrow(regseason1314))) {
  opp <- regseason1314$Opponent[i]
  day <- regseason1314$Date[i]
  opp_record <- filter(dtd_records1314, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records1314 <- c(opp_win_records1314, opp_record)
}

regseason1314 <- regseason1314 %>%
  mutate(opp_win_percent = opp_win_records1314)

regseason1314 <- regseason1314 %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

ratings1314 <- get_general(
  season = 2014,
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

regseason1314 <- merge(x = regseason1314, y = ratings1314,
                       by = "Team")

regseason1314 <- merge(x = regseason1314, y = ratings1314,
                       by.x = "Opponent", by.y = "Team")

regseason1314 <- regseason1314 %>%
  rename(off_rating = "off_rating.x") %>%
  rename(def_rating = "def_rating.x") %>%
  rename(net_rating = "net_rating.x") %>%
  rename(pace = "pace.x") %>%
  rename(opp_off_rating = "off_rating.y") %>%
  rename(opp_def_rating = "def_rating.y") %>%
  rename(opp_net_rating = "net_rating.y") %>%
  rename(opp_pace = "pace.y") 

regseason1314 <- regseason1314 %>%
  mutate(net_rating_diff = net_rating - opp_net_rating) %>%
  mutate(pace_diff = pace - opp_pace)


write_csv(regseason1314, "/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1314.csv")





