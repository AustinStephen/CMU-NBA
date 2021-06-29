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

# Info about flight distance, time, direction, coordinates
travel1617 <- nba_travel(start_season = 2017,
                              end_season = 2017,
                              return_home = 3,
                              phase = "RS") #airball

# Data about back to back, first or second leg etc. 
density1617 <- nba_density(travel1617) #airball

#this data has tipoff time
schedule1617 <- get_schedule(2016) %>%
  filter(season_type == "REG") #NBAr
#main useful variable from schedule1617 is game_time_utc. Havent actually merged schedule1617 with anything yet


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density <- merge(x = travel1617, y = density1617,
                       by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away")) %>%
  mutate("Date" = as.character(Date)) %>%
  filter(Rest != 15)


regseason1617_a <- mutate(travel_and_density, Win = (travel_and_density$"W/L" == "W")) %>%
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
flight_durations = c()
for (d in regseason1617_a$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations = c(flight_durations, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations = c(flight_durations, hours)
  }
  else {
    flight_durations = c(flight_durations, 0)
  }
}

regseason1617_b <- mutate(regseason1617_a, flight_duration = flight_durations) %>%
  select(- "Flight Time")
  
team_boxscores1617 <- get_team_boxscore(2016) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name)

regseason1617 <- merge(x = regseason1617_b, y = team_boxscores1617,
                       by = c("Date", "Team")) %>%
  select(-c("season_year", "team_abbreviation", "game_date", "matchup", "wl", "min")) %>%
  rename(score_diff = plus_minus)


write_csv(regseason1617, "/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1617.csv")
  
colnames(regseason1617)



init_lm <- lm(score_diff ~ Distance + flight_duration + B2B_2nd  + Visitor ,
              data = regseason1617)

#to do: need to add home team current record, opponent current record

#could also try doing a logistic regression model with win probability as response

#suffers from OVB rn

summary(init_lm)












  
