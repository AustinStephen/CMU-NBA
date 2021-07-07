data1617 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1617.csv")

data1718 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1718.csv")

data1819 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1819.csv")


together <- rbind(data1617, data1718)

together <- rbind(together, data1819)

visitors <- filter(together, Visitor == TRUE)


test_rest_visitors <- select(visitors, c(Rest, adjusted_score_diff)) %>%
  group_by(Rest) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))
#interesting to see that teams do well with 1-2 days rest but poor after more than that
# huge dip after 7 days rest. Likely due to small sample size

test_shift_visitors <- select(visitors, c(shift, adjusted_score_diff)) %>%
  group_by(shift) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))
#Nothing convincing here. Might need more seasons of data

test_34_visitors <- select(visitors, c(three_in_four, adjusted_score_diff)) %>%
  group_by(three_in_four) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))

test_b2b_visitors <- select(visitors, c(b2b_2nd, adjusted_score_diff)) %>%
  group_by(b2b_2nd) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))

visitors_lm <- lm(adjusted_score_diff ~ win_percent_diff, 
                  data = visitors)

summary(visitors_lm)

# Distance flight_duration  Rest 
# shift   b2b_2nd   three_in_four   Visitor
# traveling_west  traveling_east
# w_lpercent opp_win_percent

#to do: opponent current record

library(ggfortify)
autoplot(visitors_lm) +
  theme_bw()

ggplot(visitors, aes(x = Rest, y= adjusted_score_diff)) +
  geom_point(alpha = 0.5)


j#Travelling west (east coast teams flying west) has a negative influence on adj_score_diff
test_west_visitor <- select(visitors, c(traveling_west, adjusted_score_diff)) %>%
  group_by(traveling_west) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))

#In contrast, travelling east is not that bad for west coast teams
test_east_visitor <- select(visitors, c(traveling_east, adjusted_score_diff)) %>%
  group_by(traveling_east) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))
