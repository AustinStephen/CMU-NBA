library(ggmap)
library(teamcolors)
library(usmap)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyverse)


data1415 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1415.csv")


cumulative_distance1415 <- data1415 %>%
  group_by(Team) %>%
  summarise_at(vars(Distance),
               list(total_distance_traveled = sum))

home_stadium_coords <- data1415 %>%
  filter(Visitor == FALSE) %>%
  select(Team, Latitude, Longitude) %>%
  distinct()

coords_and_distance <- merge(x = cumulative_distance1415,  y= home_stadium_coords,
                             by = "Team")

highlight <- coords_and_distance %>%
  filter(Team %in% c("Portland Trail Blazers","Denver Nuggets", 
                     "Cleveland Cavaliers", "Boston Celtics"))

usa <- map_data("usa")
states <- map_data("state")
ggplot() + geom_polygon(data = states, aes(x=long, y = lat, fill = region, group = group), fill = "azure2", color = "gray") + 
  coord_fixed(1.3) +
  geom_point(data = coords_and_distance, aes(x=Longitude, y=Latitude,  
                                             size = sqrt(total_distance_traveled)),
             alpha = 0.8) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        legend.position = "bottom") +
  geom_point(data = highlight, aes(x=Longitude, y=Latitude,colour = "red", size = sqrt(total_distance_traveled))) +
  geom_text(data = coords_and_distance, aes(x=Longitude, y=Latitude, 
                                            label=ifelse(Team %in% c("Portland Trail Blazers","Denver Nuggets", 
                                         "Cleveland Cavaliers", "Boston Celtics"),
                             as.character(Team),'')),hjust= 1,vjust=0)

  
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


# Flight Maps -------------------------------------------------------------
library(airball)

datos <- nba_travel(start_season = 2015, end_season = 2015)

nba_travel_plot(data = datos,
                season = 2015,
                team = c("Portland Trail Blazers","Denver Nuggets", "Cleveland Cavaliers", "Boston Celtics"),
                city_color = "white",
                plot_background_fill = "black",
                land_color = "gray",
                caption_color = "lightblue",
                ncolumns = 2)
