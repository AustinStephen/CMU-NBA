library(devtools)
library(airball)
library(nbastatR)
library(tidyverse)
library(ggplot2)
library(echarts4r)
library(echarts4r.assets) 
library(NBAr)
library(lubridate)
library(ballr)

data1011 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1011.csv")
data1112 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1112.csv")
data1213 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1213.csv")
data1314 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1314.csv")
data1415 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1415.csv")
data1516 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1516.csv")
data1617 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1617.csv")
data1718 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1718.csv")
data1819 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1819.csv")

together <- rbind(data1011, data1112)
together <- rbind(together, data1213)
together <- rbind(together, data1314)
together <- rbind(together, data1415)
together <- rbind(together, data1516)
together <- rbind(together, data1617)
together <- rbind(together, data1718)
together <- rbind(together, data1819)

capped_score_diffs <- c() 
for (i in c(1:nrow(together))) {
  sd = together$score_diff[i]
  if (sd < (-30)) {
    sd = -30
  }
  else if (sd > 30) {
    sd = 30
  }
  capped_score_diffs <- c(capped_score_diffs, sd)
}

together <- together %>%
  mutate(capped_score_diff = capped_score_diffs) %>%
  mutate(hours_shift = as.factor(shift))

together$hours_shift <-relevel(together$hours_shift, ref= "0")

together <- together %>%
  select(-c(gp, "w.y", "l.y", min, w_pct, plus_minus:cfparams))

together <- together %>% 
  mutate(off_rating_diff = off_rating - opp_off_rating) %>%
  mutate(def_rating_diff = def_rating - opp_def_rating) %>%
  mutate(fgm_diff = fgm - opp_fgm) %>%
  mutate(fg_pct_diff = fg_pct - opp_fg_pct) %>%
  mutate(fg3m_diff = fg3m - opp_fg3m) %>%
  mutate(fg3_pct_diff = fg3_pct - opp_fg3_pct) %>%
  mutate(ftm_diff = ftm - opp_ftm) %>%
  mutate(ft_pct_diff = ft_pct - opp_ft_pct) %>%
  mutate(reb_diff = reb - opp_reb) %>%
  mutate(ast_diff = ast- opp_ast) %>%
  mutate(tov_diff = tov - opp_tov) %>%
  mutate(stl_diff = stl - opp_stl) %>%
  mutate(blk_diff = blk - opp_blk) %>%
  mutate(win_percent_diff = win_percent_diff *100)
  
write_csv(together, "/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/together.csv")


# Data manipulations above. Load below ------------------------------------
together <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/together.csv")

visitors <- filter(together, Visitor == TRUE)

# Constructing a Linear Model ---------------------------------------------
rows <- sample(nrow(together))
temp <- together[rows, ]
together_sample <- temp[!duplicated(temp$game_id),]

no_duplicates <- together[!duplicated(together$game_id),]

init_lm <- lm(score_diff ~ win_percent_diff + net_rating_diff + Visitor + Rest + hours_shift, 
              data = no_duplicates )

summary(init_lm)

# This model does not use duplicates --------------------------------------
performance_lm <- lm(score_diff ~ win_percent_diff + net_rating_diff + fg_pct_diff + 
                       fg3m_diff + ftm_diff  + 
                       reb_diff + ast_diff + stl_diff + blk_diff + tov_diff,
                     data = no_duplicates)  

summary(performance_lm)

performance_visitors_lm <- lm(score_diff ~ win_percent_diff + fg_pct_diff + 
                                fg3m_diff + ftm_diff + 
                                 reb_diff + tov_diff + stl_diff + blk_diff,
                              data = visitors) 

summary(performance_visitors_lm)

#STRONGEST MODEL SO FAR
# Visitors Model ----------------------------------------------------------
travel_visitors_lm <- lm(score_diff ~ win_percent_diff + Distance + Rest
                         + hours_shift + three_in_four + b2b_2nd, 
                  data = visitors)

summary(travel_visitors_lm)


# Together Model ----------------------------------------------------------
#Likely do not use this model becuase double counting games potentially leads to wacky results
#Ask Max about this
together_lm <- lm(score_diff ~ win_percent_diff + net_rating_diff + Visitor + Rest + hours_shift, 
                  data = together)

summary(together_lm)


# EDA --------------------------------------------------------------------
ggplot(together, aes(x = as.factor(Rest), y= score_diff)) +
  geom_violin() +
  geom_boxplot(width = 0.2)

ggplot(together, aes(x = flight_duration, y = score_diff)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

check <- lm(score_diff ~ hours_shift, data = together)

summary(check)

test_rest_visitors <- select(visitors, c(Rest, net_rating_diff)) %>%
  group_by(Rest) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

test_shift_visitors <- select(visitors, c(shift, net_rating_diff)) %>%
  group_by(shift) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

test_b2b_visitors <- select(visitors, c(b2b_2nd, net_rating_diff)) %>%
  group_by(b2b_2nd) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

test_34_visitors <- select(visitors, c(three_in_four, net_rating_diff)) %>%
  group_by(three_in_four) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

test_45_visitors <- select(visitors, c(four_in_five, net_rating_diff)) %>%
  group_by(four_in_five) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

test_57_visitors <- select(visitors, c(five_in_seven, net_rating_diff)) %>%
  group_by(five_in_seven) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

test_west_visitor <- select(visitors, c(traveling_west, net_rating_diff)) %>%
  group_by(traveling_west) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

#In contrast, traveling east is bad for west teams. Still small numbers
test_east_visitor <- select(visitors, c(traveling_east, net_rating_diff)) %>%
  group_by(traveling_east) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_net_rating_diff = mean))

# Score Diff --------------------------------------------------------------
test_rest_visitors_score_diff <- select(visitors, c(Rest, score_diff)) %>%
  group_by(Rest) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff = mean))
#interesting to see that teams do well with 1 days rest but poor after more than that

test_shift_visitors_score_diff <- select(visitors, c(shift, adjusted_score_diff)) %>%
  group_by(shift) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_score_diff = mean))
#Nothing convincing here. Might need more seasons of data

test_b2b_visitors_score_diff <- select(visitors, c(b2b_2nd, adjusted_score_diff)) %>%
  group_by(b2b_2nd) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_score_diff = mean))

test_34_visitors_score_diff <- select(visitors, c(three_in_four, adjusted_score_diff)) %>%
  group_by(three_in_four) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_score_diff = mean))

test_45_visitors_score_diff <- select(visitors, c(four_in_five, adjusted_score_diff)) %>%
  group_by(four_in_five) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_score_diff = mean))

test_57_visitors_score_diff <- select(visitors, c(five_in_seven, adjusted_score_diff)) %>%
  group_by(five_in_seven) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_score_diff = mean))

#the numbers here are super tiny
test_west_visitor_score_diff <- select(visitors, c(traveling_west, score_diff)) %>%
  group_by(traveling_west) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff = mean))

#In contrast, traveling east is bad for west teams. Still small numbers
test_east_visitor_score_diff <- select(visitors, c(traveling_east, score_diff)) %>%
  group_by(traveling_east) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff = mean))

ggplot(visitors, aes(x = as.factor(three_in_four), y= score_diff)) +
  geom_violin() +
  geom_boxplot(width = 0.2)

ggplot(visitors, aes(x = off_rating - opp_off_rating, y = score_diff)) +
  geom_point(alpha = 0.3)

ggplot(visitors, aes(y = adjusted_score_diff, x = win_percent_diff)) +
  geom_point(alpha = 0.3)

ggplot(visitors, aes(x = pace_diff, y = score_diff)) +
  geom_point(alpha = 0.3)

ggplot(visitors, aes(x = def_rating - opp_def_rating, y = score_diff)) +
  geom_point(alpha = 0.4)

ggplot(visitors, aes(x = net_rating_diff, y= score_diff)) +
  geom_point(alpha = 0.3)

ggplot(visitors, aes(x = Distance, y = net_rating_diff)) +
  geom_point(alpha = 0.3)

ggplot(visitors, aes(x = flight_duration, y = score_diff)) +
  geom_point(alpha = 0.3)

# expected_point_diff = home_team_season_average - away_team_season_average
# actual_point_diff = score_diff  
# deviation_from_expected_diff = actual_point_diff - expected_point_diff

#adj = actual_score_diff - season_avg_score_diff_at_home_or-away

#facet by season, have teams gotten smarter over the years such that travel, shift, rest is less of a problem




