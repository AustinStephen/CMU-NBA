library(ggmap)
library(teamcolors)
library(usmap)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyverse)


data1617 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1617.csv")


cumulative_distance1617 <- data1617 %>%
  group_by(Team) %>%
  summarise_at(vars(Distance),
               list(total_distance_traveled = sum))

home_stadium_coords <- data1617 %>%
  filter(Visitor == FALSE) %>%
  select(Team, Latitude, Longitude) %>%
  distinct()

coords_and_distance <- merge(x = cumulative_distance1617,  y= home_stadium_coords,
                             by = "Team")


usa <- map_data("usa")
states <- map_data("state")
ggplot() + geom_polygon(data = states, aes(x=long, y = lat, fill = region, group = group), fill = "azure2", color = "gray") + 
  coord_fixed(1.3) +
  geom_point(data = coords_and_distance, aes(x=Longitude, y=Latitude,  
                                             size = total_distance_traveled),
             alpha = 0.8) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        legend.position = "bottom")
  
  #color = Team,
                                             #fill = Team)) + 
  #scale_fill_teams(guide = FALSE) +
  #scale_color_teams(4, guide = FALSE)

  

league_pal("nba")

teamcolors %>%
  filter(grepl("New ", name)) %>%
  pull(logo) %>%
  knitr::include_graphics()

test <- teamcolors %>%
  group_by(league) %>%
  summarize(
    num_teams = n(),
    num_logos = sum(!is.na(logo))
  )



  
  







#every away team, distribution of score differential, bracketed by the time zone shift

