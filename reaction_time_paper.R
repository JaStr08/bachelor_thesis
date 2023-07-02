#' This script is used for the reaction analysis in the paper. First the data is
#' prepared so that I have all the necessary variables before setting up the
#' regression models.

rm(list=ls())

library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)
library(stargazer)
library(plm)

source("helper_functions.R")

### Load Twitter Data
df_simple_data <- read_csv('Data/Processed/simple.csv')
df_simple_data$party_name <- factor(df_simple_data$party_name, levels = c('Republican', 'Democrat'))


### Filter out corrupted data (goal to get the meta data for each Tweet again)
df_simple_data <- df_simple_data %>%
  filter(favorite_count > 0 | retweet_count < 5)


### Add categorical reaction variable
df_simple_data <- filter(df_simple_data, election_type != 'Governor') %>%
  mutate(aftermath = ifelse(post_delta_days <= 20, TRUE, FALSE)) %>%
  mutate(direct_aftermath = ifelse(post_delta_days <= 5, TRUE, FALSE))


# ---- Simple Data ----
df_simple_regression_climate <- filter(df_simple_data, topic == 'climate_change')
df_simple_regression_gun <- filter(df_simple_data, topic == 'gun_policy')
df_simple_regression_immigration <- filter(df_simple_data, topic == 'immigration')


# Analysis Climate Change ----
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


reg1 <- lm(post_delta_days ~ campaign_period + party_name + election_type, 
           data = df_simple_regression_first_tweet_climate)
summary(reg1)
cov1 <- vcovHC(reg1, type = "HC1")
robust1 <- sqrt(diag(cov1))

reg2 <- lm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + state, 
           data = df_simple_regression_first_tweet_climate)
cov2 <- vcovHC(reg2, type = "HC1")
robust2 <- sqrt(diag(cov2))
summary(reg2)

reg3 <- lm(direct_aftermath ~ campaign_period + party_name + election_type,
           data = df_simple_regression_first_tweet_climate)
cov3 <- vcovHC(reg3, type = "HC1")
robust3 <- sqrt(diag(cov3))
summary(reg3)

reg4 <- lm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
           data = df_simple_regression_first_tweet_climate)
cov4 <- vcovHC(reg4, type = "HC1")
robust4 <- sqrt(diag(cov4))
summary(reg4)

reg5 <- lm(aftermath ~ campaign_period + party_name + election_type,
           data = df_simple_regression_first_tweet_climate)
cov5 <- vcovHC(reg5, type = "HC1")
robust5 <- sqrt(diag(cov5))
summary(reg5)

reg6 <- lm(aftermath ~ campaign_period + party_name*election_type*campaign_period,
           data = df_simple_regression_first_tweet_climate)
cov6 <- vcovHC(reg6, type = "HC1")
robust6 <- sqrt(diag(cov6))
summary(reg6)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6,
          se = list(robust1, robust2, robust3, robust4, robust5, robust6), 
          title = 'Results Climate Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')



# Analysis Gun Policy ----
df_simple_regression_gun_copy <- df_simple_regression_gun
df_simple_regression_first_tweet_gun <- tibble()
event_dates_gun <- sort(unique(filter(df_simple_regression_gun, post_delta_days == 0)$created_at))
for (i in seq(length(event_dates_gun), 1)) {
  current_df <- filter(df_simple_regression_gun_copy, ymd(created_at) >= ymd(event_dates_gun[i]))
  df_simple_regression_gun_copy <- filter(df_simple_regression_gun_copy, ymd(created_at) < ymd(event_dates_gun[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == min(created_at))
  df_simple_regression_first_tweet_gun <- rbind(df_simple_regression_first_tweet_gun, current_df)
}
rm(df_simple_regression_gun_copy)


reg1 <- lm(post_delta_days ~ campaign_period + party_name + election_type, 
           data = df_simple_regression_first_tweet_gun)
summary(reg1)
cov1 <- vcovHC(reg1, type = "HC1")
robust1 <- sqrt(diag(cov1))

reg2 <- lm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + state, 
           data = df_simple_regression_first_tweet_gun)
cov2 <- vcovHC(reg2, type = "HC1")
robust2 <- sqrt(diag(cov2))
summary(reg2)

reg3 <- lm(direct_aftermath ~ campaign_period + party_name + election_type,
           data = df_simple_regression_first_tweet_gun)
cov3 <- vcovHC(reg3, type = "HC1")
robust3 <- sqrt(diag(cov3))
summary(reg3)

reg4 <- lm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
           data = df_simple_regression_first_tweet_gun)
cov4 <- vcovHC(reg4, type = "HC1")
robust4 <- sqrt(diag(cov4))
summary(reg4)

reg5 <- lm(aftermath ~ campaign_period + party_name + election_type,
           data = df_simple_regression_first_tweet_gun)
cov5 <- vcovHC(reg5, type = "HC1")
robust5 <- sqrt(diag(cov5))
summary(reg5)

reg6 <- lm(aftermath ~ campaign_period + party_name*election_type*campaign_period,
           data = df_simple_regression_first_tweet_gun)
cov6 <- vcovHC(reg6, type = "HC1")
robust6 <- sqrt(diag(cov6))
summary(reg6)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6,
          se = list(robust1, robust2, robust3, robust4, robust5, robust6), 
          title = 'Results Climate Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')




# Analysis Immigration ----
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

reg1 <- lm(post_delta_days ~ campaign_period + party_name + election_type, 
           data = df_simple_regression_first_tweet_immigration)
summary(reg1)
cov1 <- vcovHC(reg1, type = "HC1")
robust1 <- sqrt(diag(cov1))

reg2 <- lm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + state, 
           data = df_simple_regression_first_tweet_immigration)
cov2 <- vcovHC(reg2, type = "HC1")
robust2 <- sqrt(diag(cov2))
summary(reg2)

reg3 <- lm(direct_aftermath ~ campaign_period + party_name + election_type,
           data = df_simple_regression_first_tweet_immigration)
cov3 <- vcovHC(reg3, type = "HC1")
robust3 <- sqrt(diag(cov3))
summary(reg3)

reg4 <- lm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
           data = df_simple_regression_first_tweet_immigration)
cov4 <- vcovHC(reg4, type = "HC1")
robust4 <- sqrt(diag(cov4))
summary(reg4)

reg5 <- lm(aftermath ~ campaign_period + party_name + election_type,
           data = df_simple_regression_first_tweet_immigration)
cov5 <- vcovHC(reg5, type = "HC1")
robust5 <- sqrt(diag(cov5))
summary(reg5)

reg6 <- lm(aftermath ~ campaign_period + party_name*election_type*campaign_period,
           data = df_simple_regression_first_tweet_immigration)
cov6 <- vcovHC(reg6, type = "HC1")
robust6 <- sqrt(diag(cov6))
summary(reg6)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6,
          se = list(robust1, robust2, robust3, robust4, robust5, robust6), 
          title = 'Results Climate Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')


# add the inclusion of topics, more frequent alone is not enough but more frequent regarding the relevant topic might do its job

# This paper examines conditional activity, topic related and in reaction to an event

