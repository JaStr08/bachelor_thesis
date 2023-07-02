rm(list=ls())

library(tidyverse)
library(lubridate)
library(sandwich)
library(stargazer)
library(plm)

source("helper_functions.R")

### Load Twitter Data
df_simple_data <- read_csv('Data/Processed/simple.csv')
df_simple_data$party_name <- factor(df_simple_data$party_name, levels = c('Republican', 'Democrat'))
df_extensive_data <- read_csv('Data/Processed/extensive.csv')
df_extensive_data$party_name <- factor(df_extensive_data$party_name, levels = c('Republican', 'Democrat'))


# Change campaign period to the period one year before an election
# df_complete_data_all_politicians <- df_complete_data_all_politicians %>%
#   mutate(campaign = ifelse(days_till_election > 170, FALSE, campaign))
# 
# df_complete_data_all <- df_complete_data_all %>%
#   mutate(campaign = ifelse(days_till_election > 170, FALSE, campaign))


### Filter out corrupted data (goal to get the meta data for each Tweet again)
df_simple_data <- df_simple_data %>%
  filter(favorite_count > 0 | retweet_count < 5)

df_extensive_data <- df_extensive_data %>%
  filter(favorite_count > 0 | retweet_count < 5)

# df_complete_data_all <- df_complete_data_all %>%
#   filter(retweet_count < 10000)


df_simple_data <- filter(df_simple_data, election_type != 'Governor') %>%
  mutate(reaction_time_categorized = ifelse(post_delta_days <= 3, 'immediate', ifelse(post_delta_days <= 30, 'aftermath', 'unrelated')))

# ---- Simple Data ----
df_simple_regression_climate <- filter(df_simple_data, topic == 'climate_change')
df_simple_regression_gun <- filter(df_simple_data, topic == 'gun_policy')
df_simple_regression_immigration <- filter(df_simple_data, topic == 'immigration')

# Analysis Climate Change ----
ggplot(df_simple_regression_climate) +
  geom_point(aes(rank(retweet_count, ties.method = 'min'), retweet_count))

df_simple_retweet_summary_climate <- df_simple_regression_climate %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_simple_regression_climate <- df_simple_regression_climate %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_climate, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_climate, first_last == first_last)$mean_retweets, TRUE, FALSE))

df_simple_regression_climate_no_retweets <- df_simple_regression_climate %>%
  filter(!grepl('\\<RT\\>', text))

reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election +
                                           party_name + as.factor(year) + state, 
                                         data = filter(df_simple_regression_climate, (year <= 2020) & (year >= 2017)), 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = filter(df_simple_regression_climate, (year <= 2020) & (year >= 2017)), 
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

#' Big Question does the days till election effect go through reaction time or is it seperate effect
#' Use categories to deal with outliers, we cannot just get rid of them cause this could invalidate our results, still I want to
#' decrease the disproportionate impact they have on the results
#' look both at above median and above mean, those measures are created by candidate to take into consideration the different characteristics
#' of the politician's Twitter account such as average reach and so on, getting a lot of Tweets always needs to be seen in relation to the
#' Tweets of the candidate but not in relation to all accounts cause then the results would depend on if large accounts publish Tweets on average closer
#' to events or not and we might have no variance in the outcome variable if we introduce candidate fixed effects

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_climate_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_climate_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_median_retweets ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_climate_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_median_retweets ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_climate_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_mean_retweets ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_climate_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_mean_retweets ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_climate_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                    + campaign*post_delta_days + as.factor(year),
                                  data = filter(df_simple_regression_climate_no_retweets, (year <= 2020) & (year >= 2017)), 
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_retweets_fixed_effects)

reg_likes_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + state + as.factor(year),
                                      data = filter(df_simple_regression_climate_no_retweets, (year <= 2020) & (year >= 2017)), 
                                      index = c('party_name'), 
                                      model = 'within')
summary(reg_likes_fixed_effects_simple)
reg_likes_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                 + campaign*post_delta_days + as.factor(year),
                               data = filter(df_simple_regression_climate_no_retweets, (year <= 2020) & (year >= 2017)), 
                               index = c('first_last'), 
                               model = 'within')
summary(reg_likes_fixed_effects)

# First Tweet Only ----
df_simple_regression_climate_copy <- df_simple_regression_climate
df_simple_regression_first_tweet_climate <- tibble()
event_dates_climate <- sort(unique(filter(df_simple_regression_climate, post_delta_days == 0)$created_at))
for (i in seq(length(event_dates_climate), 1)) {
  current_df <- filter(df_simple_regression_climate_copy, ymd(created_at) >= ymd(event_dates_climate[i]))
  df_simple_regression_climate_copy <- filter(df_simple_regression_climate_copy, ymd(created_at) < ymd(event_dates_climate[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == min(created_at))
  df_simple_regression_first_tweet_climate <- rbind(df_simple_regression_first_tweet_climate, current_df)
}
rm(df_simple_regression_climate_copy)

df_simple_regression_first_tweet_climate_no_retweets <- df_simple_regression_first_tweet_climate %>%
  filter(!grepl('\\<RT\\>', text))

reg_first_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election +
                                                 as.factor(year), 
                                               data = filter(df_simple_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + campaign + campaign*days_till_election +
                                          as.factor(year), 
                                        data = filter(df_simple_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

reg_first_retweets_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + as.factor(year),
                                               data = filter(df_simple_regression_first_tweet_climate_no_retweets, (year <= 2020) & (year >= 2017)), 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_retweets_fixed_effects_simple)
reg_first_retweets_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                          + campaign*post_delta_days + as.factor(year),
                                        data = filter(df_simple_regression_first_tweet_climate_no_retweets, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_retweets_fixed_effects)

reg_first_likes_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                            data = filter(df_simple_regression_first_tweet_climate_no_retweets, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_likes_fixed_effects_simple)
reg_first_likes_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                       + campaign*post_delta_days + as.factor(year),
                                     data = filter(df_simple_regression_first_tweet_climate_no_retweets, (year <= 2020) & (year >= 2017)), 
                                     index = c('first_last'), 
                                     model = 'within')
summary(reg_first_likes_fixed_effects)


# Analysis Gun Policy ----
ggplot(df_simple_regression_gun) +
  geom_point(aes(rank(retweet_count, ties.method = 'min'), retweet_count))

df_simple_retweet_summary_gun <- df_simple_regression_gun %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_simple_regression_gun <- df_simple_regression_gun %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_gun, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_gun, first_last == first_last)$mean_retweets, TRUE, FALSE))

df_simple_regression_gun_no_retweets <- df_simple_regression_gun %>%
  filter(!grepl('\\<RT\\>', text))

reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election +
                                           party_name + as.factor(year) + state, 
                                         data = filter(df_simple_regression_gun, (year <= 2020) & (year >= 2017)), 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = filter(df_simple_regression_gun, (year <= 2020) & (year >= 2017)), 
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

#' Big Question does the days till election effect go through reaction time or is it seperate effect

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_gun_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_gun_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_median_retweets ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_gun_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_median_retweets ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_gun_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_mean_retweets ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_gun_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_mean_retweets ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_gun_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_likes_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                      data = df_regression_all_tweets_gun,
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_likes_fixed_effects_simple)
reg_likes_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                 + campaign*post_delta_days + as.factor(year),
                               data = df_regression_all_tweets_gun, 
                               index = c('first_last'), 
                               model = 'within')
summary(reg_likes_fixed_effects)


# First Tweet Only ----
df_regression_all_tweets_gun_copy <- df_regression_all_tweets_gun
df_regression_first_tweet_gun <- tibble()
for (i in seq(length(event_dates_gun), 1)) {
  current_df <- filter(df_regression_all_tweets_gun_copy, ymd(created_at) >= ymd(event_dates_gun[i]))
  df_regression_all_tweets_gun_copy <- filter(df_regression_all_tweets_gun_copy, ymd(created_at) < ymd(event_dates_gun[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == max(created_at))
  df_regression_first_tweet_gun <- rbind(df_regression_first_tweet_gun, current_df)
}
rm(df_regression_all_tweets_gun_copy)

reg_first_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + first_last +
                                                 as.factor(year), 
                                               data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                               index = c('first_last'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + campaign + campaign*days_till_election +
                                          as.factor(year), 
                                        data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

reg_first_retweets_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + as.factor(year),
                                               data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                               index = c('first_last'), 
                                               model = 'within')
summary(reg_first_retweets_fixed_effects_simple)
reg_first_retweets_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                          + campaign*post_delta_days + as.factor(year),
                                        data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_retweets_fixed_effects)

reg_first_likes_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                            data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_likes_fixed_effects_simple)
reg_first_likes_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                       + campaign*post_delta_days + as.factor(year),
                                     data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                     index = c('first_last'), 
                                     model = 'within')
summary(reg_first_likes_fixed_effects)



# Graphs ----
df_regression_all_tweets <- rbind(df_regression_all_tweets_climate,
                                  df_regression_all_tweets_gun)


# Graph showing the amount of Tweets to a certain topic n days after an event related to that topic ----
df_post_delta_days_distribution <- df_regression_all_tweets %>%
  group_by(post_delta_days, topic) %>%
  summarise(n = n()) 

df_total_per_group <- df_politicians %>%
  filter(office_title %in% c('Senator', 'Senate Candidate')) %>%
  group_by(party_name) %>%
  summarise(n_total = n())

df_post_delta_days_distribution <- df_post_delta_days_distribution %>%
  mutate(share = n / ifelse(party_name == df_total_per_group$party_name[1], 
                            df_total_per_group$n_total[1],
                            df_total_per_group$n_total[2]))

ggplot(df_post_delta_days_distribution, aes(post_delta_days, share, color = party_name)) +
  geom_point()

ggplot(df_regression_all_tweets, aes(post_delta_days)) +
  geom_histogram()


# First Tweet of each person/account only ----
regression_df_copy <- df_regression_all_tweets
df_regression_first_tweet <- tibble()
for (i in seq(length(event_dates), 1)) {
  current_df <- filter(regression_df_copy, ymd(created_at) >= ymd(event_dates[i]))
  regression_df_copy <- filter(regression_df_copy, ymd(created_at) < ymd(event_dates[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == max(created_at))
  df_regression_first_tweet <- rbind(df_regression_first_tweet, current_df)
}

reg_first_retweets <- lm(retweet_count ~ post_delta_days + year + party_name + followers + followers*year, df_regression_first_tweet)
summary(reg_first_retweets)

reg_first_likes <- lm(favorite_count ~ post_delta_days + year + party_name + followers + followers*year, df_regression_first_tweet)
summary(reg_first_likes)


# Analysis Immigration ----
ggplot(df_simple_regression_immigration) +
  geom_point(aes(rank(retweet_count, ties.method = 'min'), retweet_count))

df_simple_retweet_summary_immigration <- df_simple_regression_immigration %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_simple_regression_immigration <- df_simple_regression_immigration %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_immigration, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_immigration, first_last == first_last)$mean_retweets, TRUE, FALSE))

df_simple_regression_immigration_no_retweets <- df_simple_regression_immigration %>%
  filter(!grepl('\\<RT\\>', text))

reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election +
                                           party_name + as.factor(year) + state, 
                                         data = filter(df_simple_regression_immigration, (year <= 2020) & (year >= 2017)), 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = filter(df_simple_regression_immigration, (year <= 2020) & (year >= 2017)), 
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_immigration_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_immigration_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_median_retweets ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_immigration_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_median_retweets ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_immigration_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_mean_retweets ~ post_delta_days + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_immigration_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects_simple <- plm(above_mean_retweets ~ reaction_time_categorized + days_till_election + state + as.factor(year),
                                         data = df_simple_regression_immigration_no_retweets, 
                                         index = c('party_name'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)

reg_retweets_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                    + campaign*post_delta_days + as.factor(year),
                                  data = filter(df_simple_regression_immigration_no_retweets, (year <= 2020) & (year >= 2017)), 
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_retweets_fixed_effects)

reg_likes_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + state + as.factor(year),
                                      data = filter(df_simple_regression_immigration_no_retweets, (year <= 2020) & (year >= 2017)), 
                                      index = c('party_name'), 
                                      model = 'within')
summary(reg_likes_fixed_effects_simple)
reg_likes_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                 + campaign*post_delta_days + as.factor(year),
                               data = filter(df_simple_regression_immigration_no_retweets, (year <= 2020) & (year >= 2017)), 
                               index = c('first_last'), 
                               model = 'within')
summary(reg_likes_fixed_effects)

# First Tweet Only ----
df_simple_regression_immigration_copy <- df_simple_regression_immigration
df_simple_regression_first_tweet_immigration <- tibble()
event_dates_immigration <- sort(unique(filter(df_simple_regression_immigration, post_delta_days == 0)$created_at))
for (i in seq(length(event_dates_immigration), 1)) {
  current_df <- filter(df_simple_regression_immigration_copy, ymd(created_at) >= ymd(event_dates_immigration[i]))
  df_simple_regression_immigration_copy <- filter(df_simple_regression_immigration_copy, ymd(created_at) < ymd(event_dates_immigration[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == min(created_at))
  df_simple_regression_first_tweet_immigration <- rbind(df_simple_regression_first_tweet_immigration, current_df)
}
rm(df_simple_regression_immigration_copy)

df_simple_regression_first_tweet_immigration_no_retweets <- df_simple_regression_first_tweet_immigration %>%
  filter(!grepl('\\<RT\\>', text))

reg_first_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election +
                                                 as.factor(year), 
                                               data = filter(df_simple_regression_first_tweet_immigration, (year <= 2020) & (year >= 2017)), 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + campaign + campaign*days_till_election +
                                          as.factor(year), 
                                        data = filter(df_simple_regression_first_tweet_immigration, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

reg_first_retweets_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + as.factor(year),
                                               data = filter(df_simple_regression_first_tweet_immigration_no_retweets, (year <= 2020) & (year >= 2017)), 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_retweets_fixed_effects_simple)
reg_first_retweets_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                          + campaign*post_delta_days + as.factor(year),
                                        data = filter(df_simple_regression_first_tweet_immigration_no_retweets, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_retweets_fixed_effects)

reg_first_likes_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                            data = filter(df_simple_regression_first_tweet_immigration_no_retweets, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_likes_fixed_effects_simple)
reg_first_likes_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                       + campaign*post_delta_days + as.factor(year),
                                     data = filter(df_simple_regression_first_tweet_immigration_no_retweets, (year <= 2020) & (year >= 2017)), 
                                     index = c('first_last'), 
                                     model = 'within')
summary(reg_first_likes_fixed_effects)




# ---- Additional Politician Information ----
df_complete_data_all_politicians <- filter(df_extensive_data, election_type != 'Governor')
df_complete_pol_information_climate <- filter(df_complete_data_all_politicians, topic == 'climate_change')
df_complete_pol_information_gun <- filter(df_complete_data_all_politicians, topic == 'gun_policy')

df_complete_data_all_politicians <- filter(df_complete_data_all_politicians, !grepl('Candidate', office_title))
df_complete_pol_information_climate <- filter(df_complete_data_all_politicians, topic == 'climate_change')
df_complete_pol_information_gun <- filter(df_complete_data_all_politicians, topic == 'gun_policy')

df_complete_data_all_politicians <- filter(df_complete_data_all_politicians, party_name == 'Democrat')
df_complete_pol_information_climate <- filter(df_complete_data_all_politicians, topic == 'climate_change')
df_complete_pol_information_gun <- filter(df_complete_data_all_politicians, topic == 'gun_policy')

# Analysis Climate Change ----
df_regression_pol_tweets_climate <- tibble()
df_complete_pol_information_climate_copy <- df_complete_pol_information_climate

for (i in seq(length(event_dates_climate), 1)) {
  current_df <- df_complete_pol_information_climate_copy %>%
    filter(ymd(created_at) >= ymd(event_dates_climate[i])) %>%
    mutate(post_delta_days = difftime(ymd(format(created_at, '%Y-%m-%d')), event_dates_climate[i], units = 'days'))
  df_complete_pol_information_climate_copy <- filter(df_complete_pol_information_climate_copy, ymd(created_at) < ymd(event_dates_climate[i]))
  df_regression_pol_tweets_climate <- rbind(df_regression_pol_tweets_climate, current_df)
} 
rm(df_complete_pol_information_climate_copy)

df_regression_pol_tweets_climate <- df_regression_pol_tweets_climate %>%
  mutate(year = as.integer(format(created_at, '%Y'))) %>%
  filter(!grepl('RT ', text, fixed = T)) %>%
  mutate(year_month = format(created_at, '%Y-%m'))

df_regression_pol_tweets_climate$post_delta_days <- as.numeric(df_regression_pol_tweets_climate$post_delta_days)
df_regression_pol_tweets_climate$days_till_election <- as.numeric(df_regression_pol_tweets_climate$days_till_election)

reg_reaction_pol_fixed_effects_simple <- plm(post_delta_days ~ days_till_election +
                                               election_type + days_till_election*election_type + as.factor(year), 
                                             data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_reaction_pol_fixed_effects_simple)
reg_reaction_pol_fixed_effects <- plm(post_delta_days ~ days_till_election + campaign + campaign*days_till_election +
                                        as.factor(year) + first_last, 
                                      data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_reaction_pol_fixed_effects)

reg_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + as.factor(year),
                                             data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_retweets_pol_fixed_effects_simple)
reg_retweets_pol_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                        campaign*post_delta_days + as.factor(year),
                                      data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_retweets_pol_fixed_effects)

reg_likes_pol_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                          data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                          index = c('first_last'), 
                                          model = 'within')
summary(reg_likes_pol_fixed_effects_simple)
reg_likes_pol_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                     + campaign*post_delta_days + as.factor(year),
                                   data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                   index = c('first_last'), 
                                   model = 'within')
summary(reg_likes_pol_fixed_effects)

# First Tweet Only ----
df_regression_pol_tweets_climate_copy <- df_regression_pol_tweets_climate
df_regression_pol_first_tweet_climate <- tibble()
for (i in seq(length(event_dates_climate), 1)) {
  current_df <- filter(df_regression_pol_tweets_climate_copy, ymd(created_at) >= ymd(event_dates_climate[i]))
  df_regression_pol_tweets_climate_copy <- filter(df_regression_pol_tweets_climate_copy, ymd(created_at) < ymd(event_dates_climate[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == max(created_at))
  df_regression_pol_first_tweet_climate <- rbind(df_regression_pol_first_tweet_climate, current_df)
}
rm(df_regression_pol_tweets_climate_copy)

reg_first_reaction_pol_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + first_last +
                                                     as.factor(year), 
                                                   data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                                   index = c('first_last'), 
                                                   model = 'within')
summary(reg_first_reaction_pol_fixed_effects_simple)
reg_first_reaction_pol_fixed_effects <- plm(post_delta_days ~ days_till_election + campaign + campaign*days_till_election +
                                              as.factor(year), 
                                            data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_reaction_pol_fixed_effects)

reg_first_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + as.factor(year),
                                                   data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                                   index = c('first_last'), 
                                                   model = 'within')
summary(reg_first_retweets_pol_fixed_effects_simple)
reg_first_retweets_pol_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                              + campaign*post_delta_days + as.factor(year),
                                            data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_retweets_pol_fixed_effects)

reg_first_likes_pol_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                                data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                                index = c('first_last'), 
                                                model = 'within')
summary(reg_first_likes_pol_fixed_effects_simple)
reg_first_likes_pol_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                           + campaign*post_delta_days + as.factor(year),
                                         data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                         index = c('first_last'), 
                                         model = 'within')
summary(reg_first_likes_pol_fixed_effects)



# Analysis Gun Policy ----
df_regression_pol_tweets_gun <- tibble()
df_complete_pol_information_gun_copy <- df_complete_pol_information_gun

for (i in seq(length(event_dates_gun), 1)) {
  current_df <- df_complete_pol_information_gun_copy %>%
    filter(ymd(created_at) >= ymd(event_dates_gun[i])) %>%
    mutate(post_delta_days = difftime(ymd(format(created_at, '%Y-%m-%d')), event_dates_gun[i], units = 'days'))
  df_complete_pol_information_gun_copy <- filter(df_complete_pol_information_gun_copy, ymd(created_at) < ymd(event_dates_gun[i]))
  df_regression_pol_tweets_gun <- rbind(df_regression_pol_tweets_gun, current_df)
} 
rm(df_complete_pol_information_gun_copy)

df_regression_pol_tweets_gun <- df_regression_pol_tweets_gun %>%
  mutate(year = as.integer(format(created_at, '%Y'))) %>%
  filter(!grepl('RT ', text, fixed = T)) %>%
  mutate(year_month = format(created_at, '%Y-%m'))

df_regression_pol_tweets_gun$post_delta_days <- as.numeric(df_regression_pol_tweets_gun$post_delta_days)
df_regression_pol_tweets_gun$days_till_election <- as.numeric(df_regression_pol_tweets_gun$days_till_election)

reg_reaction_pol_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + first_last +
                                               as.factor(year), 
                                             data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_reaction_pol_fixed_effects_simple)
reg_reaction_pol_fixed_effects <- plm(post_delta_days ~ days_till_election + campaign + campaign*days_till_election +
                                        as.factor(year), 
                                      data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_reaction_pol_fixed_effects)

summary(lm(retweet_count ~ post_delta_days + days_till_election + as.factor(year), filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017))))
reg_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + as.factor(year),
                                             data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_retweets_pol_fixed_effects_simple)
reg_retweets_pol_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                        post_delta_days*party_name + election_type + as.factor(year),
                                      data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_retweets_pol_fixed_effects)
summary(lm(retweet_count ~ post_delta_days + days_till_election + party_name + post_delta_days*party_name + as.factor(year), filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017))))

reg_likes_pol_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                          data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                          index = c('first_last'), 
                                          model = 'within')
summary(reg_likes_pol_fixed_effects_simple)
reg_likes_pol_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                     + campaign*post_delta_days + as.factor(year),
                                   data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                   index = c('first_last'), 
                                   model = 'within')
summary(reg_likes_pol_fixed_effects)

ggplot(filter(filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), favorite_count != 0)) +
  geom_histogram(aes(x = retweet_count, color = party_name))

# Graphs ----
df_regression_all_tweets <- rbind(df_regression_all_tweets_gun,
                                  df_regression_all_tweets_gun)


# Graph showing the amount of Tweets to a certain topic n days after an event related to that topic ----
df_post_delta_days_distribution <- df_regression_all_tweets %>%
  group_by(party_name, post_delta_days) %>%
  summarise(n = n()) 

df_total_per_group <- df_politicians %>%
  filter(office_title %in% c('Senator', 'Senate Candidate')) %>%
  group_by(party_name) %>%
  summarise(n_total = n())

df_post_delta_days_distribution <- df_post_delta_days_distribution %>%
  mutate(share = n / ifelse(party_name == df_total_per_group$party_name[1], 
                            df_total_per_group$n_total[1],
                            df_total_per_group$n_total[2]))

ggplot(df_post_delta_days_distribution, aes(post_delta_days, share, color = party_name)) +
  geom_point()

ggplot(df_regression_all_tweets, aes(post_delta_days)) +
  geom_histogram()


# First Tweet Only ----
df_regression_pol_tweets_gun_copy <- df_regression_pol_tweets_gun
df_regression_pol_first_tweet_gun <- tibble()
for (i in seq(length(event_dates_gun), 1)) {
  current_df <- filter(df_regression_pol_tweets_gun_copy, ymd(created_at) >= ymd(event_dates_gun[i]))
  df_regression_pol_tweets_gun_copy <- filter(df_regression_pol_tweets_gun_copy, ymd(created_at) < ymd(event_dates_gun[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == max(created_at))
  df_regression_pol_first_tweet_gun <- rbind(df_regression_pol_first_tweet_gun, current_df)
}
rm(df_regression_pol_tweets_gun_copy)

summary(lm(post_delta_days ~ days_till_election + as.factor(year), filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017))))
reg_first_reaction_pol_fixed_effects_simple <- plm(post_delta_days ~ days_till_election +
                                                     as.factor(year), 
                                                   data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                                   index = c('first_last'), 
                                                   model = 'within')
summary(reg_first_reaction_pol_fixed_effects_simple)
reg_first_reaction_pol_fixed_effects <- plm(post_delta_days ~ days_till_election + campaign + campaign*days_till_election +
                                              as.factor(year), 
                                            data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_reaction_pol_fixed_effects)

summary(lm(retweet_count ~ post_delta_days + days_till_election + as.factor(year), filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017))))
reg_first_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ post_delta_days + days_till_election + as.factor(year),
                                                   data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                                   index = c('first_last'), 
                                                   model = 'within')
summary(reg_first_retweets_pol_fixed_effects_simple)
reg_first_retweets_pol_fixed_effects <- plm(retweet_count ~ post_delta_days + days_till_election + campaign + 
                                              + campaign*post_delta_days + as.factor(year),
                                            data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_retweets_pol_fixed_effects)

reg_first_likes_pol_fixed_effects_simple <- plm(favorite_count ~ post_delta_days + days_till_election + as.factor(year),
                                                data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                                index = c('first_last'), 
                                                model = 'within')
summary(reg_first_likes_pol_fixed_effects_simple)
reg_first_likes_pol_fixed_effects <- plm(favorite_count ~ post_delta_days + days_till_election + campaign + 
                                           + campaign*post_delta_days + as.factor(year),
                                         data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                         index = c('first_last'), 
                                         model = 'within')
summary(reg_first_likes_pol_fixed_effects)




