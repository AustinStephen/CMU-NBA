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

regseason1617_travel <- nba_travel(start_season = 2017,
                              end_season = 2017,
                              return_home = 3,
                              phase = "RS") #airball

regseason1617_density <- nba_density(regseason1617_travel) #airball

regseason1617_schedule <- get_schedule(2016) %>%
  filter(season_type == "REG") #NBAr

regseason1617 <- merge(x = regseason1617_travel, y = regseason1617_density,
                       by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away")) %>%
  #mutate(Win = (regseason1617$"W/L" == "W")) %>%
  filter(Rest != 15)

regseason1617 <- mutate(regseason1617, Win = (regseason1617$"W/L" == "W")) %>%
  select(c("Season", "Month", "Week", "Date", "Team", "Opponent", "Visitor", "City", "Win", 
           "Distance", "Route", "Rest", "TZ", "Shift (hrs)", "Flight Time", "Direction (E/W)", 
           "Latitude", "Longitude", "B2B", "B2B-1st", "B2B-2nd", "3in4", "4in5", "5in7"))
  

#TODO get score difference data, flight time, current records

gamelist <- c(21600001 : 21601230)
traditional <- map(gamelist, ~get_boxscore(.,boxscore_type = 'traditional')) %>% compact() %>%  bind_rows()





  
