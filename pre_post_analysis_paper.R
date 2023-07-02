#' This script is used to generate the pre-/post-analysis. Next to the results
#' in the paper. The implied date analysis is included which still needs further
#' optimization so it did not end up in the initial paper, but basically here
#' event are based on the general Tweet frequency of specific topics.

library(tidyverse)
library(Microsoft365R)
library(lubridate)
library(sandwich)
library(stargazer)
library(plm)

rm(list = ls())

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

### Chose Parameter ----
# select number of days to display in graph before the day of event
number_days_pre <- 10
# select number of days to display in graph after the day of event
number_days_post <- 15

df_simple_data <- read_csv('Data/Processed/simple.csv') %>%
  filter(election_type != 'Governor')
df_extensive_data <- read_csv('Data/Processed/extensive.csv') %>%
  filter(election_type != 'Governor')

#' Data Gap Data
df_gap_overview_senate <- read_csv('Data/Data_Gaps/senate_gap_overview_combined.csv')
df_gap_overview_house <- read_csv('Data/Data_Gaps/house_gap_overview_combined.csv')
df_gap_overview <- rbind(df_gap_overview_house, df_gap_overview_senate)

#' Accounts Per Month Data
df_accounts_per_month <- read_csv('Data/Data_Summary/number_accounts_by_election_month.csv') %>%
  filter(twitter_data == 'Combined Accounts') %>%
  select(c('year_month', 'count', 'office_type'))
names(df_accounts_per_month)[names(df_accounts_per_month) %in% c('count', 'office_type')] <- c('account_count', 'election_type')

df_simple <- df_simple_data %>%
  mutate(pre_post_delta_days = ifelse(post_delta_days <= number_days_post, post_delta_days, NA)) %>%
  mutate(pre_post_delta_days = ifelse((pre_delta_days <= number_days_pre & is.na(pre_post_delta_days)), -pre_delta_days, pre_post_delta_days)) %>%
  mutate(implied_pre_post_delta_days = ifelse(implied_post_delta_days <= number_days_post, implied_post_delta_days, NA)) %>%
  mutate(implied_pre_post_delta_days = ifelse((implied_pre_delta_days <= number_days_pre & is.na(implied_pre_post_delta_days)), -implied_pre_delta_days, implied_pre_post_delta_days)) %>%
  group_by(topic, election_type, pre_post_delta_days, implied_pre_post_delta_days, party_name, created_at) %>%
  dplyr::summarise(number_daily_tweets = n()) %>%
  mutate(year_month = format(created_at, '%Y-%m')) %>%
  filter(!created_at %in% dates_to_exclude) %>%
  inner_join(df_accounts_per_month) %>%
  mutate(number_daily_tweets_to_accounts_ratio = number_daily_tweets/account_count)

first_events <- df_simple %>%
  filter(pre_post_delta_days == 0) %>%
  group_by(topic) %>%
  summarise(first_event_date = min(created_at))

last_events <- df_simple %>%
  filter(pre_post_delta_days == 0) %>%
  group_by(topic) %>%
  summarise(last_event_date = max(created_at))

df_simple <- df_simple %>%
  group_by(topic) %>%
  inner_join(first_events) %>%
  inner_join(last_events) %>%
  filter(created_at >= first_event_date) %>%
  filter(created_at <= last_event_date)

# ---- Democrats vs. Republicans ----
### Predefined Dates ----
df_to_plot <- df_simple %>%
  filter(!created_at %in% dates_to_exclude) %>%
  group_by(topic, election_type, pre_post_delta_days, party_name) %>%
  dplyr::summarise(mean_daily_tweets_to_accounts_ratio = mean(number_daily_tweets_to_accounts_ratio)) %>%
  mutate(after = ifelse(pre_post_delta_days >= 0, TRUE, FALSE))

df_to_plot$party_name <- factor(df_to_plot$party_name, levels = c('Republican', 'Democrat'))

df_to_plot_after <- filter(df_to_plot, after == TRUE)
df_to_plot_before <- filter(df_to_plot, after == FALSE)

topic.labels <- c('Climate Policy', 'Gun Policy', 'Immigration Policy')
names(topic.labels) <- c('climate_change', 'gun_policy', 'immigration')
ggplot() +
  geom_line(aes(pre_post_delta_days, mean_daily_tweets_to_accounts_ratio, color = party_name), df_to_plot) +
  geom_smooth(aes(pre_post_delta_days, mean_daily_tweets_to_accounts_ratio, color = party_name, fill = party_name), df_to_plot_after, method = 'lm', alpha = 0.3) +
  geom_smooth(aes(pre_post_delta_days, mean_daily_tweets_to_accounts_ratio, color = party_name, fill = party_name), df_to_plot_before, method = 'lm', alpha = 0.3) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(name=expression(Delta*"Days To Climate Event"), breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y',
             labeller = labeller(topic = topic.labels)) +
  theme_light() +
  theme(legend.position = 'top',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_discrete(name=NULL) +
  scale_fill_discrete(NULL)
  

### Implied Dates ----
df_to_plot_implied <- df_simple %>%
  group_by(topic, election_type, implied_pre_post_delta_days, party_name) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_daily_tweets))

ggplot(df_to_plot_implied) +
  geom_line(aes(implied_pre_post_delta_days, mean_daily_tweets, color = party_name)) +
  scale_x_continuous(name='\u0394 Days To Climate Event', breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y') +
  theme_light() +
  theme(legend.position = 'top') +
  scale_color_discrete(name=NULL)


# ---- Candidates vs. Winners ----
#' bad quality data not usuable in paper
df_simple_candidate <- df_simple_data %>%
  mutate(candidate_only = ifelse(grepl('Candidate', office_title), TRUE, FALSE)) %>%
  mutate(pre_post_delta_days = ifelse(post_delta_days <= number_days_post, post_delta_days, NA)) %>%
  mutate(pre_post_delta_days = ifelse((pre_delta_days <= number_days_pre & is.na(pre_post_delta_days)), -pre_delta_days, pre_post_delta_days)) %>%
  mutate(implied_pre_post_delta_days = ifelse(implied_post_delta_days <= number_days_post, implied_post_delta_days, NA)) %>%
  mutate(implied_pre_post_delta_days = ifelse((implied_pre_delta_days <= number_days_pre & is.na(implied_pre_post_delta_days)), -implied_pre_delta_days, implied_pre_post_delta_days)) %>%
  group_by(topic, election_type, pre_post_delta_days, implied_pre_post_delta_days, candidate_only, created_at) %>%
  dplyr::summarise(number_daily_tweets = n())

### Predefined Dates ----
df_to_plot_candidate <- df_simple_candidate %>%
  group_by(topic, election_type, pre_post_delta_days, candidate_only) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_daily_tweets))

df_to_plot_candidate_after <- filter(df_to_plot, after == TRUE)
df_to_plot_before <- filter(df_to_plot, after == FALSE)

ggplot(df_to_plot_candidate) +
  geom_line(aes(pre_post_delta_days, mean_daily_tweets, color = candidate_only)) +
  scale_x_continuous(name='\u0394 Days To Climate Event', breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y') +
  theme_light() +
  theme(legend.position = 'top') +
  scale_color_discrete(name=NULL)

### Implied Dates ----
df_to_plot_candidate_implied <- df_simple_candidate %>%
  group_by(topic, election_type, implied_pre_post_delta_days, candidate_only) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_daily_tweets))

ggplot(df_to_plot_candidate_implied) +
  geom_line(aes(implied_pre_post_delta_days, mean_daily_tweets, color = candidate_only)) +
  scale_x_continuous(name='\u0394 Days To Climate Event', breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y') +
  theme_light() +
  theme(legend.position = 'top') +
  scale_color_discrete(name=NULL)

df_extensive_data %>%
  mutate(candidate_only = ifelse(grepl('Candidate', office_title), TRUE, FALSE)) %>%
  group_by(candidate_only) %>%
  summarise(n_accounts = length(unique(first_last)))


# ---- Campaign vs. Non-Campaign ----
df_extensive_campaign <- df_extensive_data %>%
  mutate(campaign = as.logical(campaign_period*in_upcoming_election)) %>%
  mutate(pre_post_delta_days = ifelse(post_delta_days <= number_days_post, post_delta_days, NA)) %>%
  mutate(pre_post_delta_days = ifelse((pre_delta_days <= number_days_pre & is.na(pre_post_delta_days)), -pre_delta_days, pre_post_delta_days)) %>%
  mutate(implied_pre_post_delta_days = ifelse(implied_post_delta_days <= number_days_post, implied_post_delta_days, NA)) %>%
  mutate(implied_pre_post_delta_days = ifelse((implied_pre_delta_days <= number_days_pre & is.na(implied_pre_post_delta_days)), -implied_pre_delta_days, implied_pre_post_delta_days)) %>%
  group_by(topic, election_type, pre_post_delta_days, implied_pre_post_delta_days, campaign, created_at) %>%
  dplyr::summarise(number_daily_tweets = n())

### Predefined Dates ----
df_to_plot_candidate <- df_extensive_campaign %>%
  group_by(topic, election_type, pre_post_delta_days, campaign) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_daily_tweets))

ggplot(df_to_plot_candidate) +
  geom_line(aes(pre_post_delta_days, mean_daily_tweets, color = campaign)) +
  scale_x_continuous(name='\u0394 Days To Climate Event', breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y') +
  theme_light() +
  theme(legend.position = 'top') +
  scale_color_discrete(name=NULL)

### Implied Dates ----
df_to_plot_candidate_implied <- df_extensive_campaign %>%
  group_by(topic, election_type, implied_pre_post_delta_days, campaign) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_daily_tweets))

ggplot(df_to_plot_candidate_implied) +
  geom_line(aes(implied_pre_post_delta_days, mean_daily_tweets, color = campaign)) +
  scale_x_continuous(name='\u0394 Days To Climate Event', breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y') +
  theme_light() +
  theme(legend.position = 'top') +
  scale_color_discrete(name=NULL)

df_extensive_data %>%
  mutate(campaign = as.logical(campaign_period*in_upcoming_election)) %>%
  group_by(campaign) %>%
  summarise(n_accounts = length(unique(first_last)))

# ---- Incumbent vs. Challenger ----
df_extensive_incumbent <- df_extensive_data %>%
  filter(in_upcoming_election == TRUE & campaign_period == TRUE) %>%
  mutate(pre_post_delta_days = ifelse(post_delta_days <= number_days_post, post_delta_days, NA)) %>%
  mutate(pre_post_delta_days = ifelse((pre_delta_days <= number_days_pre & is.na(pre_post_delta_days)), -pre_delta_days, pre_post_delta_days)) %>%
  mutate(implied_pre_post_delta_days = ifelse(implied_post_delta_days <= number_days_post, implied_post_delta_days, NA)) %>%
  mutate(implied_pre_post_delta_days = ifelse((implied_pre_delta_days <= number_days_pre & is.na(implied_pre_post_delta_days)), -implied_pre_delta_days, implied_pre_post_delta_days)) %>%
  group_by(topic, election_type, pre_post_delta_days, implied_pre_post_delta_days, incumbent, created_at) %>%
  dplyr::summarise(number_daily_tweets = n())

### Predefined Dates ----
df_to_plot_candidate <- df_extensive_incumbent %>%
  group_by(topic, election_type, pre_post_delta_days, incumbent) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_daily_tweets))

ggplot(df_to_plot_candidate) +
  geom_line(aes(pre_post_delta_days, mean_daily_tweets, color = incumbent)) +
  scale_x_continuous(name='\u0394 Days To Climate Event', breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y') +
  theme_light() +
  theme(legend.position = 'top') +
  scale_color_discrete(name=NULL)

### Implied Dates ----
df_to_plot_candidate_implied <- df_extensive_incumbent %>%
  group_by(topic, election_type, implied_pre_post_delta_days, incumbent) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_daily_tweets))

ggplot(df_to_plot_candidate_implied) +
  geom_line(aes(implied_pre_post_delta_days, mean_daily_tweets, color = incumbent)) +
  scale_x_continuous(name='\u0394 Days To Climate Event', breaks = seq(-number_days_pre, number_days_post, 1)) +
  scale_y_continuous(name = 'Mean of Daily Tweets') +
  facet_grid(election_type ~ topic, scales = 'free_y') +
  theme_light() +
  theme(legend.position = 'top') +
  scale_color_discrete(name=NULL)
