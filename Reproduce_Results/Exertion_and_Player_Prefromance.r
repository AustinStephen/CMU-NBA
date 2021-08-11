## Author: Austin Stephen 
## Date: 8/11/2021
## Purpose: Self contained code to reproduce the results 
#           for paper section: "Exertion and Player Performance" 


## libraries
library(nbastatR)
library(tidyverse)

## Reading in the tracking data



## FG percentage 

##fgpct all distance
gbg_player_w_distance_at <-  gbg_player_w_distance %>%
  filter(fga > 0 )

gbg_player_w_distance %>%
  ggplot(aes(x=diff_from_sn_avg_fgpct, y =diff_from_sn_avg_dist ))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")

model <- lm(diff_from_sn_avg_fgpct ~ diff_from_sn_avg_dist, 
            data = gbg_player_w_distance_at )
summary(model)





