#Bubble Season
library(devtools)
library(airball)
library(nbastatR)
library(tidyverse)
library(ggplot2)
library(echarts4r)
library(echarts4r.assets) 
library(nbastatR)
library(ballr)
library(NBAr)
library(lubridate)
library(ballr)


# Info about flight distance, time, direction, coordinates
bubble_travel <- nba_travel(start_season = 2020,
                         end_season = 2020,
                         return_home = 3,
                         phase = c("RS", "PO"))

bubble_travel <- filter(bubble_travel, Date > "2020-08-01") %>%
  arrange(Date)
  #airball 

# Data about back to back, first or second leg etc. 
bubble_density <- nba_density(bubble_travel) #airball


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
bubble_travel_and_density <- merge(x = bubble_travel, y = bubble_density,
                                by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away")) %>%
  filter(Rest != 15)

bubble <- mutate(bubble_travel_and_density, Win = (bubble_travel_and_density$"W/L" == "W")) %>%
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
bubble_flight_durations = c()

for (d in bubble$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    bubble_flight_durations = c(bubble_flight_durations, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    bubble_flight_durations = c(bubble_flight_durations, hours)
  }
  else {
    bubble_flight_durations = c(bubble_flight_durations, 0)
  }
}

bubble <- mutate(bubble, flight_duration = bubble_flight_durations) %>%
  select(- "Flight Time")

bubble_team_boxscores_reg <- get_team_boxscore(2019) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus))

bubble_team_boxscores_playoffs <- get_team_boxscore(2019, season_type = "Playoffs") %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus))

bubble_team_boxscores <- rbind(bubble_team_boxscores_reg, bubble_team_boxscores_playoffs) %>%
  arrange(Date)


bubble <- merge(x = bubble, y = bubble_team_boxscores,
                       by = c("Date", "Team")) %>%
  rename(score_diff = plus_minus) %>%
  mutate(traveling_west = (direction == "West")) %>% 
  mutate(traveling_east = (direction == "East")) %>%
  mutate(B2B = (B2B == "Yes")) %>%
  mutate(b2b_1st = (b2b_1st == "Yes")) %>%
  mutate(b2b_2nd = (b2b_2nd == "Yes")) %>%
  mutate(three_in_four = (three_in_four == "Yes")) %>%
  mutate(four_in_five = (four_in_five == "Yes")) %>%
  mutate(five_in_seven = (five_in_seven == "Yes")) %>%
  filter(Week > 31) %>%
  arrange(Date)

avg_score_diffs <- bubble %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff = mean))

bubble <- merge(x = bubble, y = avg_score_diffs, 
                       by = "Team")

#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs_bubble = c()

for (i in c(1:nrow(bubble))) {
  adj = bubble$score_diff[i] - bubble$avg_score_diff[i]
  adj_score_diffs_bubble = c(adj_score_diffs_bubble, adj)
}

bubble <- bubble %>%
  mutate(adjusted_score_diff = adj_score_diffs_bubble)

dtd_records_bubble <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/record_by_day_2019_20_season.csv")

dtd_records_bubble$Team <- stringr::str_replace(dtd_records_bubble$team, '\\*', '') 

dtd_records_bubble <- select(dtd_records_bubble, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

bubble <- merge(x= bubble, y = dtd_records_bubble,
                       by = c("Date", "Team")) %>%
  filter(Rest <= 3)

opp_win_records_bubble <- c()
for (i in c(1:nrow(bubble))) {
  opp <- bubble$Opponent[i]
  day <- bubble$Date[i]
  opp_record <- filter(dtd_records_bubble, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records_bubble <- c(opp_win_records_bubble, opp_record)
}

bubble <- bubble %>%
  mutate(opp_win_percent = opp_win_records_bubble)

bubble <- bubble %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

write_csv(bubble, "/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/bubble.csv")








