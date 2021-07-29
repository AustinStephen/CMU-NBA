## PURPOSE: transform the windows dataset to build the proxy for team stength
## AUTHOR: Austin
library(readr)
library(tidyverse)
library(dplyr)

together_net_Rating_allWindows <- read_csv(
  "data/proxy_team_strength/together_net_Rating_allCenters.csv") %>%
  select(-c("w_lpercent","win_percent_diff")) %>%
  # select(c("Team","Opponent", "Date", "game_id","season", "net_rating_diff","opp_net_rating",
  #          "w_lpercent", "game_net_rating", "net_rating_2gameWindow", "net_rating_5gameWindow",
  #          "net_rating_7gameWindow", "net_rating_10gameWindow", "opp_net_rating_2gameWindow",
  #          "opp_net_rating_5gameWindow","opp_net_rating_7gameWindow","opp_net_rating_10gameWindow",
  #         "opp_net_rating_CentergameWindow", "net_rating_CentergameWindow")) %>%
  mutate(wind_2 = net_rating_2gameWindow - opp_net_rating_2gameWindow,
         wind_5 = net_rating_5gameWindow - opp_net_rating_5gameWindow,
         wind_7 = net_rating_7gameWindow - opp_net_rating_7gameWindow,
         wind_10 = net_rating_10gameWindow - opp_net_rating_10gameWindow,
         wind_15 = net_rating_15gameWindow - opp_net_rating_15gameWindow,
         wind_20 = net_rating_20gameWindow - opp_net_rating_20gameWindow,
         wind_25 = net_rating_25gameWindow - opp_net_rating_25gameWindow,
         wind_30 = net_rating_30gameWindow - opp_net_rating_30gameWindow,
         wind_35 = net_rating_35gameWindow - opp_net_rating_35gameWindow,
         wind_40 = net_rating_40gameWindow - opp_net_rating_40gameWindow,
         wind_10_center = net_rating_Center10gameWindow - opp_net_rating_Center10gameWindow,
         wind_20_center = net_rating_Center20gameWindow - opp_net_rating_Center20gameWindow,
         wind_30_center = net_rating_Center30gameWindow - opp_net_rating_Center30gameWindow,
         wind_40_center = net_rating_Center40gameWindow - opp_net_rating_Center40gameWindow,
         wind_50_center = net_rating_Center50gameWindow - opp_net_rating_Center50gameWindow,
         wind_60_center = net_rating_Center60gameWindow - opp_net_rating_Center60gameWindow)


## The hollow windows 
together_net_Rating_allWindows <- together_net_Rating_allWindows %>%
  mutate( 
    ## 5
    hollow_wind5_home = net_rating_5gameWindow - net_rating_2gameWindow,
    hollow_wind5_opp = opp_net_rating_5gameWindow - opp_net_rating_2gameWindow,
    wind_5_hollow = hollow_wind5_home - hollow_wind5_opp,
    
    ## 7
    hollow_wind7_home = net_rating_7gameWindow - net_rating_5gameWindow,
    hollow_wind7_opp = opp_net_rating_7gameWindow - opp_net_rating_5gameWindow,
    wind_7_hollow = hollow_wind7_home - hollow_wind7_opp,
    
    ## 10 -5
    hollow_wind10_home_5 = net_rating_10gameWindow - net_rating_5gameWindow,
    hollow_wind10_opp_5 = opp_net_rating_10gameWindow - opp_net_rating_5gameWindow,
    wind_10_hollow_5 = hollow_wind10_home_5 - hollow_wind10_opp_5,
    
    ## 10 - 7
    hollow_wind10_home = net_rating_10gameWindow - net_rating_7gameWindow,
    hollow_wind10_opp = opp_net_rating_10gameWindow - opp_net_rating_7gameWindow,
    wind_10_hollow = hollow_wind10_home - hollow_wind10_opp,
    
    ## 15
    hollow_wind15_home = net_rating_15gameWindow - net_rating_10gameWindow,
    hollow_wind15_opp = opp_net_rating_15gameWindow - opp_net_rating_10gameWindow,
    wind_15_hollow = hollow_wind15_home - hollow_wind15_opp,
    
    ## 20
    hollow_wind20_home = net_rating_20gameWindow - net_rating_15gameWindow,
    hollow_wind20_opp = opp_net_rating_20gameWindow - opp_net_rating_15gameWindow,
    wind_20_hollow = hollow_wind20_home - hollow_wind20_opp,
    
    ## 25
    hollow_wind25_home = net_rating_25gameWindow - net_rating_20gameWindow,
    hollow_wind25_opp = opp_net_rating_25gameWindow - opp_net_rating_20gameWindow,
    wind_25_hollow = hollow_wind25_home - hollow_wind25_opp,
    
    ## 30
    hollow_wind30_home = net_rating_30gameWindow - net_rating_25gameWindow,
    hollow_wind30_opp = opp_net_rating_30gameWindow - opp_net_rating_25gameWindow,
    wind_30_hollow = hollow_wind30_home - hollow_wind30_opp,
    
    ## 35
    hollow_wind35_home = net_rating_35gameWindow - net_rating_30gameWindow,
    hollow_wind35_opp = opp_net_rating_35gameWindow - opp_net_rating_30gameWindow,
    wind_35_hollow = hollow_wind35_home - hollow_wind35_opp,
    
    ## 40
    hollow_wind40_home = net_rating_40gameWindow - net_rating_35gameWindow,
    hollow_wind40_opp = opp_net_rating_40gameWindow - opp_net_rating_35gameWindow,
    wind_40_hollow = hollow_wind40_home - hollow_wind40_opp,
    
    ## centered 20
    hollow_wind20_cent_home = net_rating_Center20gameWindow - net_rating_Center10gameWindow,
    hollow_wind20_cent_opp = opp_net_rating_Center20gameWindow - opp_net_rating_Center10gameWindow,
    hollow_wind20_cent = hollow_wind20_cent_home - hollow_wind20_cent_opp,
    
    ## centered 30
    hollow_wind30_cent_home = net_rating_Center30gameWindow - net_rating_Center20gameWindow,
    hollow_wind30_cent_opp = opp_net_rating_Center30gameWindow - opp_net_rating_Center20gameWindow,
    hollow_wind30_cent = hollow_wind30_cent_home - hollow_wind30_cent_opp,
    
    ## centered 40
    hollow_wind40_cent_home = net_rating_Center40gameWindow - net_rating_Center30gameWindow,
    hollow_wind40_cent_opp = opp_net_rating_Center40gameWindow - opp_net_rating_Center30gameWindow,
    hollow_wind40_cent = hollow_wind40_cent_home - hollow_wind40_cent_opp,
    
    ## centered 50
    hollow_wind50_cent_home = net_rating_Center50gameWindow - net_rating_Center40gameWindow,
    hollow_wind50_cent_opp = opp_net_rating_Center50gameWindow - opp_net_rating_Center40gameWindow,
    hollow_wind50_cent = hollow_wind50_cent_home - hollow_wind50_cent_opp,
    
    ## centered 60
    hollow_wind60_cent_home = net_rating_Center60gameWindow - net_rating_Center50gameWindow,
    hollow_wind60_cent_opp = opp_net_rating_Center60gameWindow - opp_net_rating_Center50gameWindow,
    hollow_wind60_cent = hollow_wind60_cent_home - hollow_wind60_cent_opp,
  )
# select(c("Team","Opponent", "Date", "game_id","season", "net_rating_diff","opp_net_rating",
#          "w_lpercent", "game_net_rating", "net_rating_2gameWindow", "net_rating_5gameWindow",
#          "net_rating_7gameWindow", "net_rating_10gameWindow", "opp_net_rating_2gameWindow",
#          "opp_net_rating_5gameWindow","opp_net_rating_7gameWindow","opp_net_rating_10gameWindow",
#         "opp_net_rating_CentergameWindow", "net_rating_CentergameWindow", 
#         "wind_30_hollow", "wind_25_hollow" ))


## making games played
together_net_Rating_allWindows <- together_net_Rating_allWindows %>% 
  group_by(Team, season) %>%
  mutate(games_played = seq_along(game_id)) %>%
  ungroup()


##Long process to make running win percent diff exclusive 

## making exclusive win percent for the home and away teams
together_net_Rating_allWindows <- together_net_Rating_allWindows %>%
  group_by("Team","season") %>%
  mutate(win_percent_home = ifelse(Visitor == FALSE,
                                   ifelse(score_diff >0,w.x-1,w.x) /pmax(1,w.x + l.x -1),
                                   0),
        win_percent_away = ifelse(Visitor == TRUE,
                                  ifelse(score_diff >0,w.x-1,w.x) /pmax(1,w.x + l.x -1),
                                  0)) %>%
  ungroup() 

## making the vistor/home rows the same for a single game id
tmp_home <- aggregate(together_net_Rating_allWindows$win_percent_home, 
            by=list(Category=together_net_Rating_allWindows$game_id), FUN=sum) %>% 
      rename(game_id = "Category", win_percent_home = "x")

tmp_away <- aggregate(together_net_Rating_allWindows$win_percent_away, 
                      by=list(Category=together_net_Rating_allWindows$game_id), FUN=sum) %>% 
  rename(game_id = "Category", win_percent_away = "x")

## dropping tmp cols
together_net_Rating_allWindows <- together_net_Rating_allWindows %>%
  select(-c("win_percent_home","win_percent_away" ))

## merging the correct columns into the main table
together_net_Rating_allWindows <- merge( x = together_net_Rating_allWindows,
                                         y =tmp_home, 
                                         by = c("game_id"))

together_net_Rating_allWindows <- merge( x = together_net_Rating_allWindows,
                                         y = tmp_away, 
                                         by = c("game_id")) 

## making the win percent diff with reference to the home team
together_net_Rating_allWindows <- together_net_Rating_allWindows %>%
  mutate(win_percent_diff =  win_percent_away - win_percent_home)

##collapsing the windowed information into 1 row for each game
together <- read_csv("data/proxy_team_strength/together.csv") %>%
  select(c("Date", "visitor"))

together <- merge(x = together, y =together_net_Rating_allWindows,
                  by.x = c("Date", "visitor"),
                  by.y = c("Date","Team"))

## writing to csv for retrieval 
together <- together %>% ungroup()%>%
  select(
    ## base cols
         "Visitor","Opponent","Date", "game_id", "season","game_net_rating",
         "win_percent_diff", "games_played", "net_rating_diff", "score_diff",
#         "wins_vis", "loss_vis",
    ## window cols
         "hollow_wind60_cent", "hollow_wind50_cent", "hollow_wind40_cent", 
         "hollow_wind30_cent", "hollow_wind20_cent", "wind_10_center", 
         "wind_40_hollow", "wind_35_hollow","wind_30_hollow", "wind_25_hollow",
         "wind_20_hollow","wind_15_hollow", "wind_10_hollow_5", "wind_5_hollow",
         "wind_7_hollow","wind_10_hollow","wind_2", "wind_5", "wind_7", "wind_10",
         "wind_15","wind_20", "wind_25", "wind_30", "wind_35", "wind_40")

write.csv(together,
          "data/proxy_team_strength/full_team_strength_columns.csv",
          row.names = FALSE)

## dropping tmp datasets 
rm(together_net_Rating_allWindows)
rm(tmp_away)
rm(tmp_home)



