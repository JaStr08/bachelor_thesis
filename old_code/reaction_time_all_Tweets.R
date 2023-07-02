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
df_extensive_data <- read_csv('Data/Processed/extensive.csv')
df_extensive_data$party_name <- factor(df_extensive_data$party_name, levels = c('Republican', 'Democrat'))


### Filter out corrupted data (goal to get the meta data for each Tweet again)
df_simple_data <- df_simple_data %>%
  filter(favorite_count > 0 | retweet_count < 5)

df_extensive_data <- df_extensive_data %>%
  filter(favorite_count > 0 | retweet_count < 5)


### Add categorical reaction variable
df_simple_data <- filter(df_simple_data, election_type != 'Governor') %>%
  mutate(aftermath = ifelse(post_delta_days <= 20, TRUE, FALSE)) %>%
  mutate(direct_aftermath = ifelse(post_delta_days <= 5, TRUE, FALSE))

df_extensive_data <- filter(df_extensive_data, election_type != 'Governor') %>%
  mutate(aftermath = ifelse(post_delta_days <= 20, TRUE, FALSE)) %>%
  mutate(direct_aftermath = ifelse(post_delta_days <= 5, TRUE, FALSE))


# ---- Simple Data ----
df_simple_regression_climate <- filter(df_simple_data, topic == 'climate_change')
df_simple_regression_gun <- filter(df_simple_data, topic == 'gun_policy')
df_simple_regression_immigration <- filter(df_simple_data, topic == 'immigration')

# Analysis Climate Change ----
df_simple_retweet_summary_climate <- df_simple_regression_climate %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_simple_regression_climate <- df_simple_regression_climate %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_climate, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_climate, first_last == first_last)$mean_retweets, TRUE, FALSE))

# Linear Scale
reg_reaction_fixed_effects_simple_linear_climate <- plm(post_delta_days ~ days_till_election + election_type*days_till_election + as.factor(year), 
                                                        data = df_simple_regression_climate, 
                                                        index = 'party_name', 
                                                        model = 'within')
summary(reg_reaction_fixed_effects_simple_linear_climate)
y# Adjust standard errors
cov_reaction_fixed_effects_simple_linear_climate <- vcovHC(reg_reaction_fixed_effects_simple_linear_climate,type="HC1",cluster="group")
robust_se_reaction_fixed_effects_simple_linear_climate <- sqrt(diag(cov_reaction_fixed_effects_simple_linear_climate))

reg_reaction_fixed_effects_linear_climate <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                                 data = df_simple_regression_climate, 
                                                 index = c('party_name'), 
                                                 model = 'within')
summary(reg_reaction_fixed_effects_linear_climate)
# Adjust standard errors
cov_reaction_fixed_effects_linear_climate <- vcovHC(reg_reaction_fixed_effects_linear_climate, type = "HC1", cluster="group")
robust_se_reaction_fixed_effects_linear_climate <- sqrt(diag(cov_reaction_fixed_effects_linear_climate))

# Categorical Scale
reg_reaction_fixed_effects_simple_categorical_climate <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                                             data = df_simple_regression_climate, 
                                                             index = 'party_name', 
                                                             model = 'within')
summary(reg_reaction_fixed_effects_simple_categorical_climate)
# Adjust standard errors
cov_reaction_fixed_effects_simple_categorical_climate <- vcovHC(reg_reaction_fixed_effects_simple_categorical_climate, type = "HC1", cluster="group")
robust_se_reaction_fixed_effects_simple_categorical_climate <- sqrt(diag(cov_reaction_fixed_effects_simple_categorical_climate))

reg_reaction_fixed_effects_categorical_climate <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                                     data = df_simple_regression_climate, 
                                                     index = c('party_name'), 
                                                     model = 'within')
summary(reg_reaction_fixed_effects_categorical_climate)
# Adjust standard errors
cov_reaction_fixed_effects_categorical_climate <- vcovHC(reg_reaction_fixed_effects_categorical_climate, type = "HC1", cluster="group")
robust_se_reaction_fixed_effects_categorical_climate <- sqrt(diag(cov_reaction_fixed_effects_categorical_climate))


# Nicely formatted summary with robust standard errors in right column
stargazer(reg_reaction_fixed_effects_simple_linear_climate, reg_reaction_fixed_effects_linear_climate,
          reg_reaction_fixed_effects_simple_categorical_climate, reg_reaction_fixed_effects_categorical_climate, 
          se = list(robust_se_reaction_fixed_effects_simple_linear_climate, robust_se_reaction_fixed_effects_linear_climate,
                    robust_se_reaction_fixed_effects_simple_categorical_climate, robust_se_reaction_fixed_effects_categorical_climate), 
          title = 'Results Climate Change', align = TRUE, style = 'aer')



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

reg_first_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + as.factor(year), 
                                               data = df_simple_regression_first_tweet_climate, 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                        data = df_simple_regression_first_tweet_climate, 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

# Categorical Scale
reg_first <- plm(post_delta_days ~ campaign_period + party_name + election_type + state, 
                                         data = df_simple_regression_first_tweet_climate, 
                                         index = 'year', 
                                         model = 'within')
summary(reg_first)
reg1 <- lm(post_delta_days ~ campaign_period + party_name + election_type, 
           data = df_simple_regression_first_tweet_climate)
summary(reg1)
cov1 <- vcovHC(reg1, type = "HC1")
robust1 <- sqrt(diag(cov1))

cov_reg_first <- vcovHC(reg_first, type = "HC1", cluster="group")
robust_reg_first <- sqrt(diag(cov_reg_first))

reg_second <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + state, 
                                  data = df_simple_regression_first_tweet_climate, 
                                  index = c('year'), 
                                  model = 'within')
summary(reg_second)
reg2 <- lm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + state, 
                  data = df_simple_regression_first_tweet_climate)
cov2 <- vcovHC(reg2, type = "HC1")
robust2 <- sqrt(diag(cov2))
summary(reg2)

cov_reg_second <- vcovHC(reg_second, type = "HC1")
robust_reg_second <- sqrt(diag(cov_reg_second))

stargazer(reg_second,
          se = list(robust_reg_second), 
          type = 'text', align = TRUE, style = 'aer',
          omit.stat = 'f')

reg_third <- plm(direct_aftermath ~ campaign_period, 
                                         data = df_simple_regression_first_tweet_climate, 
                                         index = 'year', 
                                         model = 'within')
summary(reg_third)
cov_reg_third <- vcovHC(reg_third, type = "HC1", cluster="group")
robust_reg_third <- sqrt(diag(cov_reg_third))

reg3 <- glm(direct_aftermath ~ campaign_period + party_name + election_type,
            family = binomial(link = 'logit'),
            data = df_simple_regression_first_tweet_climate)
reg3 <- lm(direct_aftermath ~ campaign_period + party_name + election_type,
            data = df_simple_regression_first_tweet_climate)
cov3 <- vcovHC(reg3, type = "HC1")
robust3 <- sqrt(diag(cov3))
summary(reg3)


reg_fourth <- plm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
                                  data = df_simple_regression_first_tweet_climate, 
                                  index = c('year'), 
                                  model = 'within')
summary(reg_fourth)
cov_reg_fourth <- vcovHC(reg_fourth, type = "HC1", cluster="group")
robust_reg_fourth <- sqrt(diag(cov_reg_fourth))
reg_fourth <- lm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
                  data = df_simple_regression_first_tweet_climate)
summary(reg_fourth)

reg4 <- glm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
            family = binomial(link = 'logit'),
            data = df_simple_regression_first_tweet_climate)
reg4 <- lm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
                 data = df_simple_regression_first_tweet_climate)
cov4 <- vcovHC(reg4, type = "HC1")
robust4 <- sqrt(diag(cov4))
summary(reg4)

reg_fifth <- plm(aftermath ~ campaign_period, 
                 data = df_simple_regression_first_tweet_climate, 
                 index = 'year', 
                 model = 'within')
summary(reg_fifth)
cov_reg_fifth <- vcovHC(reg_fifth, type = "HC1", cluster="group")
robust_reg_fifth <- sqrt(diag(cov_reg_fifth))

reg5 <- glm(direct_aftermath ~ campaign_period + party_name + election_type,
            family = binomial(link = 'logit'),
            data = df_simple_regression_first_tweet_climate)
reg5 <- lm(direct_aftermath ~ campaign_period + party_name + election_type,
            data = df_simple_regression_first_tweet_climate)
cov5 <- vcovHC(reg5, type = "HC1")
robust5 <- sqrt(diag(cov5))


reg_sixth <- plm(aftermath ~ campaign_period + party_name*election_type*campaign_period, 
                  data = df_simple_regression_first_tweet_climate, 
                  index = c('year'), 
                  model = 'within')
summary(reg_sixth)
cov_reg_sixth <- vcovHC(reg_sixth, type = "HC1", cluster="group")
robust_reg_sixth <- sqrt(diag(cov_reg_sixth))

reg6 <- glm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
            family = binomial(link = 'logit'),
            data = df_simple_regression_first_tweet_climate)
reg6 <- lm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period,
            data = df_simple_regression_first_tweet_climate)
cov6 <- vcovHC(reg6, type = "HC1")
robust6 <- sqrt(diag(cov6))

stargazer(reg_first, reg_second, reg_third, reg_fourth, reg_fifth, reg_sixth,
          se = list(robust_reg_first, robust_reg_second, robust_reg_third, 
                    robust_reg_fourth, robust_reg_fifth, robust_reg_sixth), 
          title = 'Results Climate Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')

stargazer(reg1, reg2, reg3, reg4, reg5, reg6,
          se = list(robust1, robust2, robust3, robust4, robust5, robust6), 
          title = 'Results Climate Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')


# Analysis Gun Policy ----
df_simple_retweet_summary_gun <- df_simple_regression_gun %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_simple_regression_gun <- df_simple_regression_gun %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_gun, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_gun, first_last == first_last)$mean_retweets, TRUE, FALSE))

# Linear Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election + as.factor(year), 
                                         data = df_simple_regression_gun, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = df_simple_regression_gun, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                         data = df_simple_regression_gun, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                  data = df_simple_regression_gun, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# First Tweet Only ----
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

# Linear Scale
reg_seventh <- plm(post_delta_days ~ campaign_period, 
                 data = df_simple_regression_first_tweet_gun, 
                 index = 'year', 
                 model = 'within')
summary(reg_seventh)
cov_reg_seventh <- vcovHC(reg_seventh, type = "HC1", cluster="group")
robust_reg_seventh <- sqrt(diag(cov_reg_seventh))

reg7 <- lm(post_delta_days ~ campaign_period + party_name + election_type, 
           data = df_simple_regression_first_tweet_climate)
summary(reg1)
cov1 <- vcovHC(reg1, type = "HC1")
robust1 <- sqrt(diag(cov1))


reg_eighth <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period, 
                  data = df_simple_regression_first_tweet_gun, 
                  index = c('year'), 
                  model = 'within')
summary(reg_eighth)
cov_reg_eigth <- vcovHC(reg_eighth, type = "HC1", cluster="group")
robust_reg_eigth <- sqrt(diag(cov_reg_eigth))

# Categorical Scale
reg_ninth <- plm(direct_aftermath ~ campaign_period, 
                 data = df_simple_regression_first_tweet_gun, 
                 index = 'year', 
                 model = 'within')
summary(reg_ninth)
cov_reg_ninth <- vcovHC(reg_ninth, type = "HC1", cluster="group")
robust_reg_ninth <- sqrt(diag(cov_reg_ninth))

reg_tenth <- plm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period, 
                  data = df_simple_regression_first_tweet_gun, 
                  index = c('year'), 
                  model = 'within')
summary(reg_tenth)
cov_reg_tenth <- vcovHC(reg_tenth, type = "HC1", cluster="group")
robust_reg_tenth <- sqrt(diag(cov_reg_tenth))

reg_eleventh <- plm(aftermath ~ campaign_period, 
                 data = df_simple_regression_first_tweet_gun, 
                 index = 'year', 
                 model = 'within')
summary(reg_eleventh)
cov_reg_eleventh <- vcovHC(reg_eleventh, type = "HC1", cluster="group")
robust_reg_eleventh <- sqrt(diag(cov_reg_eleventh))

reg_twelveth <- plm(aftermath ~ campaign_period + party_name*election_type*campaign_period, 
                 data = df_simple_regression_first_tweet_gun, 
                 index = c('year'), 
                 model = 'within')
summary(reg_twelveth)
cov_reg_twelveth <- vcovHC(reg_twelveth, type = "HC1", cluster="group")
robust_reg_twelveth <- sqrt(diag(cov_reg_twelveth))

stargazer(reg_seventh, reg_eighth, reg_ninth, reg_tenth, reg_eleventh, reg_twelveth,
          se = list(robust_reg_seventh, robust_reg_eigth, robust_reg_ninth, 
                    robust_reg_tenth, robust_reg_eleventh, robust_reg_twelveth), 
          title = 'Results Gun Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')


# Analysis Immigration ----
df_simple_retweet_summary_immigration <- df_simple_regression_immigration %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_simple_regression_immigration <- df_simple_regression_immigration %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_immigration, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_simple_retweet_summary_immigration, first_last == first_last)$mean_retweets, TRUE, FALSE))

# Linear Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election + as.factor(year), 
                                         data = df_simple_regression_immigration, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = df_simple_regression_immigration, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                         data = df_simple_regression_immigration, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                  data = df_simple_regression_immigration, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

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

# Linear Scale
reg_thirteenth <- plm(post_delta_days ~ campaign_period, 
                   data = df_simple_regression_first_tweet_immigration, 
                   index = 'year', 
                   model = 'within')
summary(reg_thirteenth)
cov_reg_thirdteenth <- vcovHC(reg_thirteenth, type = "HC1", cluster="group")
robust_reg_thirteenth <- sqrt(diag(cov_reg_thirdteenth))

reg_fourteenth <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period, 
                  data = df_simple_regression_first_tweet_immigration, 
                  index = c('year'), 
                  model = 'within')
summary(reg_fourteenth)
cov_reg_fourteenth <- vcovHC(reg_fourteenth, type = "HC1", cluster="group")
robust_reg_fourteenth <- sqrt(diag(cov_reg_fourteenth))

# Categorical Scale
reg_fifteenth <- plm(direct_aftermath ~ campaign_period, 
                 data = df_simple_regression_first_tweet_immigration, 
                 index = 'year', 
                 model = 'within')
summary(reg_fifteenth)
cov_reg_fifteenth <- vcovHC(reg_fifteenth, type = "HC1", cluster="group")
robust_reg_fifteenth <- sqrt(diag(cov_reg_fifteenth))

reg_sixteenth <- plm(direct_aftermath ~ campaign_period + party_name*election_type*campaign_period, 
                 data = df_simple_regression_first_tweet_immigration, 
                 index = c('year'), 
                 model = 'within')
summary(reg_sixteenth)
cov_reg_sixteenth <- vcovHC(reg_sixteenth, type = "HC1", cluster="group")
robust_reg_sixteenth <- sqrt(diag(cov_reg_sixteenth))

reg_seventheenth <- plm(aftermath ~ campaign_period, 
                    data = df_simple_regression_first_tweet_immigration, 
                    index = 'year', 
                    model = 'within')
summary(reg_seventheenth)
cov_reg_seventeenth <- vcovHC(reg_seventheenth, type = "HC1", cluster="group")
robust_reg_seventeenth <- sqrt(diag(cov_reg_seventeenth))

reg_eigthteenth <- plm(aftermath ~ campaign_period + party_name*election_type*campaign_period, 
                    data = df_simple_regression_first_tweet_immigration, 
                    index = c('year'), 
                    model = 'within')
summary(reg_eigthteenth)
cov_reg_eighteenth <- vcovHC(reg_eigthteenth, type = "HC1", cluster="group")
robust_reg_eighteenth <- sqrt(diag(cov_reg_eighteenth))

stargazer(reg_thirteenth, reg_fourteenth, reg_fifteenth, reg_sixteenth, reg_seventheenth, reg_eigthteenth,
          se = list(robust_reg_thirteenth, robust_reg_fourteenth, robust_reg_fifteenth, 
                    robust_reg_sixteenth, robust_reg_seventeenth, robust_reg_eighteenth), 
          title = 'Results Gun Policy', align = TRUE, style = 'aer',
          omit.stat = 'f')






# ---- Additional Politician Information ----
df_extensive_data <- df_extensive_data %>%
  filter(!is.na(days_till_election))
df_extensive_regression_climate <- filter(df_extensive_data, topic == 'climate_change')
df_extensive_regression_gun <- filter(df_extensive_data, topic == 'gun_policy')
df_extensive_regression_immigration <- filter(df_extensive_data, topic == 'immigration')

# Analysis Climate Change ----
df_extensive_retweet_summary_climate <- df_extensive_regression_climate %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_extensive_regression_climate <- df_extensive_regression_climate %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_extensive_retweet_summary_climate, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_extensive_retweet_summary_climate, first_last == first_last)$mean_retweets, TRUE, FALSE))

# Linear Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election + as.factor(year), 
                                         data = df_extensive_regression_climate, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = df_extensive_regression_climate, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_mode + election_type*campaign_mode + as.factor(year), 
                                         data = df_extensive_regression_climate, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_mode + party_name*election_type*campaign_mode + as.factor(year), 
                                  data = df_extensive_regression_climate, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# First Tweet Only ----
df_extensive_regression_climate_copy <- df_extensive_regression_climate
df_extensive_regression_first_tweet_climate <- tibble()
event_dates_climate <- sort(unique(filter(df_extensive_regression_climate, post_delta_days == 0)$created_at))
for (i in seq(length(event_dates_climate), 1)) {
  current_df <- filter(df_extensive_regression_climate_copy, ymd(created_at) >= ymd(event_dates_climate[i]))
  df_extensive_regression_climate_copy <- filter(df_extensive_regression_climate_copy, ymd(created_at) < ymd(event_dates_climate[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == min(created_at))
  df_extensive_regression_first_tweet_climate <- rbind(df_extensive_regression_first_tweet_climate, current_df)
}
rm(df_extensive_regression_climate_copy)

reg_first_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + as.factor(year), 
                                               data = df_extensive_regression_first_tweet_climate, 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                        data = df_extensive_regression_first_tweet_climate, 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                         data = df_extensive_regression_first_tweet_climate, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                  data = df_extensive_regression_first_tweet_climate, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)



# Analysis Gun Policy ----
df_extensive_retweet_summary_gun <- df_extensive_regression_gun %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_extensive_regression_gun <- df_extensive_regression_gun %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_extensive_retweet_summary_gun, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_extensive_retweet_summary_gun, first_last == first_last)$mean_retweets, TRUE, FALSE))

# Linear Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election + as.factor(year), 
                                         data = df_extensive_regression_gun, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = df_extensive_regression_gun, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                         data = df_extensive_regression_gun, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                  data = df_extensive_regression_gun, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# First Tweet Only ----
df_extensive_regression_gun_copy <- df_extensive_regression_gun
df_extensive_regression_first_tweet_gun <- tibble()
event_dates_gun <- sort(unique(filter(df_extensive_regression_gun, post_delta_days == 0)$created_at))
for (i in seq(length(event_dates_gun), 1)) {
  current_df <- filter(df_extensive_regression_gun_copy, ymd(created_at) >= ymd(event_dates_gun[i]))
  df_extensive_regression_gun_copy <- filter(df_extensive_regression_gun_copy, ymd(created_at) < ymd(event_dates_gun[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == min(created_at))
  df_extensive_regression_first_tweet_gun <- rbind(df_extensive_regression_first_tweet_gun, current_df)
}
rm(df_extensive_regression_gun_copy)

reg_first_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + as.factor(year), 
                                               data = df_extensive_regression_first_tweet_gun, 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                        data = df_extensive_regression_first_tweet_gun, 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                         data = df_extensive_regression_first_tweet_gun, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                  data = df_extensive_regression_first_tweet_gun, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)


# Analysis Immigration ----
df_extensive_retweet_summary_immigration <- df_extensive_regression_immigration %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))

df_extensive_regression_immigration <- df_extensive_regression_immigration %>%
  mutate(above_median_retweets = ifelse(retweet_count > filter(df_extensive_retweet_summary_immigration, first_last == first_last)$median_retweets, TRUE, FALSE)) %>%
  mutate(above_mean_retweets = ifelse(retweet_count > filter(df_extensive_retweet_summary_immigration, first_last == first_last)$mean_retweets, TRUE, FALSE))

# Linear Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + election_type*days_till_election + as.factor(year), 
                                         data = df_extensive_regression_immigration, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                  data = df_extensive_regression_immigration, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                         data = df_extensive_regression_immigration, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                  data = df_extensive_regression_immigration, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

# First Tweet Only ----
df_extensive_regression_immigration_copy <- df_extensive_regression_immigration
df_extensive_regression_first_tweet_immigration <- tibble()
event_dates_immigration <- sort(unique(filter(df_extensive_regression_immigration, post_delta_days == 0)$created_at))
for (i in seq(length(event_dates_immigration), 1)) {
  current_df <- filter(df_extensive_regression_immigration_copy, ymd(created_at) >= ymd(event_dates_immigration[i]))
  df_extensive_regression_immigration_copy <- filter(df_extensive_regression_immigration_copy, ymd(created_at) < ymd(event_dates_immigration[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == min(created_at))
  df_extensive_regression_first_tweet_immigration <- rbind(df_extensive_regression_first_tweet_immigration, current_df)
}
rm(df_extensive_regression_immigration_copy)

reg_first_reaction_fixed_effects_simple <- plm(post_delta_days ~ days_till_election + as.factor(year), 
                                               data = df_extensive_regression_first_tweet_immigration, 
                                               index = c('party_name'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(post_delta_days ~ days_till_election + party_name*election_type*days_till_election + as.factor(year), 
                                        data = df_extensive_regression_first_tweet_immigration, 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

# Categorical Scale
reg_reaction_fixed_effects_simple <- plm(post_delta_days ~ campaign_period + election_type*campaign_period + as.factor(year), 
                                         data = df_extensive_regression_first_tweet_immigration, 
                                         index = 'first_last', 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(post_delta_days ~ campaign_period + party_name*election_type*campaign_period + as.factor(year), 
                                  data = df_extensive_regression_first_tweet_immigration, 
                                  index = c('party_name'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)












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



