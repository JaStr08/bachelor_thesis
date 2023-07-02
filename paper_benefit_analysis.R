#' This script is used to create the final analysis of the benefits as can be
#' found in the according section in the paper.

rm(list=ls())

library(tidyverse)
library(lubridate)
library(sandwich)
library(stargazer)
library(plm)

source("helper_functions.R")

# important dates
state_of_the_union <- c(ymd('2010-01-27'), ymd('2011-01-25'), ymd('2012-01-24'), ymd('2013-02-12'),
                        ymd('2014-01-28'), ymd('2015-01-20'), ymd('2016-01-12'), ymd('2017-02-28'),
                        ymd('2018-01-30'), ymd('2019-02-05'), ymd('2020-02-04'), ymd('2021-04-28'),
                        ymd('2022-03-01'), ymd('2023-02-07')) + days(1)
world_earth_day <- seq(ymd('2010-04-22'), ymd('2023-04-22'), 'years')
gun_violence_awareness_day <- c(ymd('2015-06-02'), ymd('2016-06-03'), ymd('2017-06-02'),
                                ymd('2018-06-01'), ymd('2019-06-07'), ymd('2020-06-05'),
                                ymd('2021-06-04'), ymd('2022-06-03'))

dates_to_exclude <- c(state_of_the_union, world_earth_day, gun_violence_awareness_day)

### Load Twitter Data
df_simple_data <- read_csv('Data/Processed/simple.csv')
df_simple_data$party_name <- factor(df_simple_data$party_name, levels = c('Republican', 'Democrat'))


### Filter out corrupted data and retweets as their retweet count is the one of the original Tweet
df_simple_data <- df_simple_data %>%
  filter(favorite_count > 0 | retweet_count < 5) %>%
  filter(!grepl('\\<RT\\>', text))


### Add Categorical Reaction Variables
df_simple_data <- filter(df_simple_data, election_type != 'Governor') %>%
  mutate(aftermath = ifelse(post_delta_days <= 20, TRUE, FALSE)) %>%
  mutate(direct_aftermath = ifelse(post_delta_days <= 5, TRUE, FALSE)) %>%
  mutate(reaction_time_categorized = ifelse(post_delta_days <= 5, 'direct_aftermath', ifelse(post_delta_days <= 20, 'aftermath', 'outside')))

df_simple_data$reaction_time_categorized <- factor(df_simple_data$reaction_time_categorized, levels = c('outside', 'direct_aftermath', 'aftermath'))

# df_simple_regression_climate <- df_simple_regression_climate %>%
#   filter(post_delta_days <= 60)

df_simple_data <- df_simple_data %>%
  filter(!created_at %in% dates_to_exclude) %>%
  filter(post_delta_days <= 75)
  # filter(!is.na(state))


### Split Data into Topics
df_simple_regression_climate <- filter(df_simple_data, topic == 'climate_change')
df_simple_regression_gun <- filter(df_simple_data, topic == 'gun_policy')
df_simple_regression_immigration <- filter(df_simple_data, topic == 'immigration')


### Add Categorical Outcome Variables
df_simple_retweet_summary_climate <- get_mean_median_retweet_count(df_simple_regression_climate)
df_simple_retweet_summary_gun <- get_mean_median_retweet_count(df_simple_regression_gun)
df_simple_retweet_summary_immigration <- get_mean_median_retweet_count(df_simple_regression_immigration)

df_simple_like_summary_climate <- get_mean_median_like_count(df_simple_regression_climate)
df_simple_like_summary_gun <- get_mean_median_like_count(df_simple_regression_gun)
df_simple_like_summary_immigration <- get_mean_median_like_count(df_simple_regression_immigration)

df_simple_regression_climate <- df_simple_regression_climate %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_climate, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_climate, first_last == first_last)$mean_retweets, TRUE, FALSE)) %>%
  mutate(above_median_likes = ifelse(favorite_count > filter(df_simple_like_summary_climate, first_last == first_last)$median_likes, TRUE, FALSE)) %>%
  mutate(above_mean_likes = ifelse(favorite_count > filter(df_simple_like_summary_climate, first_last == first_last)$mean_likes, TRUE, FALSE))

df_simple_regression_gun <- df_simple_regression_gun %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_gun, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_gun, first_last == first_last)$mean_retweets, TRUE, FALSE)) %>%
  mutate(above_median_likes = ifelse(favorite_count > filter(df_simple_like_summary_gun, first_last == first_last)$median_likes, TRUE, FALSE)) %>%
  mutate(above_mean_likes = ifelse(favorite_count > filter(df_simple_like_summary_gun, first_last == first_last)$mean_likes, TRUE, FALSE))

df_simple_regression_immigration <- df_simple_regression_immigration %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_immigration, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_immigration, first_last == first_last)$mean_retweets, TRUE, FALSE)) %>%
  mutate(above_median_likes = ifelse(favorite_count > filter(df_simple_like_summary_immigration, first_last == first_last)$median_likes, TRUE, FALSE)) %>%
  mutate(above_mean_likes = ifelse(favorite_count > filter(df_simple_like_summary_immigration, first_last == first_last)$mean_likes, TRUE, FALSE))

rm(df_simple_retweet_summary_climate, df_simple_retweet_summary_gun, df_simple_retweet_summary_immigration,
   df_simple_like_summary_climate, df_simple_like_summary_gun, df_simple_like_summary_immigration)

# Climate Policy ----
reg1 <- plm(retweet_count ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_climate, 
            index = c('year'), 
            model = 'within')
summary(reg1)

reg2 <- plm(above_median_likes ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_climate, 
            index = c('year'), 
            model = 'within')
summary(reg2)
cov2 <- vcovHC(reg2, type = "HC1", cluster="group")
robust2 <- sqrt(diag(cov2))

reg3 <- plm(above_mean_likes ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_climate, 
            index = c('year'), 
            model = 'within')
summary(reg3)
cov3 <- vcovHC(reg3, type = "HC1", cluster="group")
robust3 <- sqrt(diag(cov3))



# Gun Policy ----
reg4 <- plm(retweet_count ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_gun, 
            index = c('year'), 
            model = 'within')
summary(reg4)

reg5 <- plm(above_median_likes ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_gun, 
            index = c('year'), 
            model = 'within')
summary(reg5)
cov5 <- vcovHC(reg5, type = "HC1", cluster="group")
robust5 <- sqrt(diag(cov5))

reg6 <- plm(above_mean_likes ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_gun, 
            index = c('year'), 
            model = 'within')
summary(reg6)
cov6 <- vcovHC(reg6, type = "HC1", cluster="group")
robust6 <- sqrt(diag(cov6))



# Immigration Policy ----
reg7 <- plm(retweet_count ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_immigration, 
            index = c('year'), 
            model = 'within')
summary(reg7)

reg8 <- plm(above_median_retweets ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_immigration, 
            index = c('year'), 
            model = 'within')
summary(reg8)
cov8 <- vcovHC(reg8, type = "HC1", cluster="group")
robust8 <- sqrt(diag(cov8))

stargazer(reg8,
          se = list(robust8), 
          type = 'text', align = TRUE, style = 'aer',
          omit.stat = 'f')

reg9 <- plm(above_mean_retweets ~ reaction_time_categorized + reaction_time_categorized*party_name*election_type + party_name + campaign_period + state,
            data = df_simple_regression_immigration, 
            index = c('year'), 
            model = 'within')
summary(reg9)
cov9 <- vcovHC(reg9, type = "HC1", cluster="group")
robust9 <- sqrt(diag(cov9))


# Create Table ----
stargazer(reg2, reg3, reg5, reg6, reg8, reg9,
          se = list(robust2, robust3, robust5, robust6, robust8, robust9), 
          title = 'Results Gun Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')


df_simple_data %>%
  group_by(topic, party_name) %>%
  summarise(n = n())

test <- read_csv('Data/Data_Summary/number_accounts_by_month.csv')
