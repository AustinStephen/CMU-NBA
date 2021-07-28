## AUTHOR: Austin
## PURPOSE: Running net diff
library(dplyr)
library(tidyverse)
library(readr)


together <- read_csv("data/between_game_dist/together.csv") %>%
  group_by(Team, season) %>%
  # select(c("Team","Opponent", "game_id","season", "net_rating_diff","opp_net_rating",
  #          "w_lpercent", "game_net_rating"))%>%
  mutate(rolling_net_rating = (cumsum(game_net_rating) - game_net_rating) / 
           pmax(1,(seq_along(game_net_rating) -1)),
         games_played = seq_along(game_net_rating)
         )

together <- together %>%
  group_by(Opponent, season) %>%
  mutate(rolling_net_rating_opp = (cumsum(-1 *game_net_rating)+
                              game_net_rating) / pmax(1,seq_along(game_net_rating) -1)) %>%
  mutate(rolling_net_rating_diff = rolling_net_rating - rolling_net_rating_opp) 


visitors <- filter(together, Visitor == TRUE, games_played > 1)


write.csv(together, "data/between_game_dist/together_mod.csv")






  

