library(ggmap)
library(teamcolors)
library(usmap)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyverse)


data1415 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1415.csv")


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
  geom_point(data = highlight, aes(x=Longitude, y=Latitude, size = sqrt(total_distance_traveled)), color = "red") +
  geom_text(data = coords_and_distance, aes(x=Longitude, y=Latitude, 
                                            label=ifelse(Team %in% c("Portland Trail Blazers","Denver Nuggets", 
                                         "Cleveland Cavaliers", "Boston Celtics"),
                             as.character(Team),'')),hjust= 0,vjust=0, size = 2.5, color = "blue")

  
  #color = Team,
                                             #fill = Team)) + 
  #scale_fill_teams(guide = FALSE) +
  #scale_color_teams(4, guide = FALSE)

library(ggimage)
library(teamcolors)


nba_logos <- filter(teamcolors, league == "nba") %>%
  select(c("name","logo")) %>%
  rename(Team = "name")

coords_and_distance <- merge(coords_and_distance, nba_logos,
                             by = "Team")
sizes <- c()
for (d in coords_and_distance$total_distance_traveled) {
  if (d >60000) {
    sizes <- c(sizes, 0.12) #99th
  }
  if (d<60000 & d> 52000) { 
    sizes <- c(sizes, 0.1)
  }
  if (d<52000 & d> 50000) {
    sizes <- c(sizes, 0.08)
  }
  if (d<50000 & d> 46000) {
    sizes <- c(sizes, 0.07)
  }
  if (d<46000 & d> 43000) {
    sizes <- c(sizes, 0.06)
  }
  if (d<43000 & d> 40000) {
    sizes <- c(sizes, 0.05)
  }
  if (d<40000) {
    sizes <- c(sizes, 0.04)
  }
}

coords_and_distance$img_size <- sizes

write_csv(coords_and_distance,"/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/coords_and_distance.csv")

ggplot() + geom_polygon(data = states, aes(x=long, y = lat, fill = region, group = group), fill = "white", color = "gray") + 
  coord_fixed(1.3) + 
  geom_image(data = coords_and_distance, aes(x = Longitude, y = Latitude, image = logo, size = I(img_size))) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        legend.position = "bottom") 


# Flight Maps -------------------------------------------------------------
library(airball)

travel1415 <- nba_travel(start_season = 2015, end_season = 2015)
write_csv(travel1415,"/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/travel1415.csv")


nba_travel_plot(data = travel1415,
                season = 2015,
                team = c("Portland Trail Blazers","Denver Nuggets", "Cleveland Cavaliers", "Boston Celtics"),
                city_color = "yellow",
                plot_background_fill = "black",
                land_color = "gray",
                caption_color = "lightblue",
                ncolumns = 2)
