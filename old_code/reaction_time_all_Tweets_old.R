# rm(list=ls())

library(tidyverse)
library(Microsoft365R)
library(lubridate)
library(sandwich)
library(stargazer)
library(plm)

source("helper_functions.R")

od_drive <- get_business_onedrive()

# potential additional topics: immigration
topics <- c('climate_change', 'gun_laws')
# predefined or implied
event_type <- 'implied'


twitter_data_selector <- 'combined'

if (twitter_data_selector == 'all') {
  tweet_dir_data_paths_od <- c("Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate/",
                               "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House/",
                               "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor/")
}

if (twitter_data_selector == 'combined') {
  tweet_dir_data_paths_od <- c("Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate_Single/",
                               "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House_Single/",
                               "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor_Single/")
}


df_complete_data_all <- read_all_files_filtered_to_df(od_drive, tweet_dir_data_paths_od, topics, twitter_data_selector)

df_complete_data_all <- df_complete_data_all %>%
  mutate(next_election_day = get_next_election_day(created_at)) %>%
  mutate(days_till_election = difftime(ymd(next_election_day), ymd(created_at), units = 'days'))

#' Add column which shows if a Tweet is created within two periods of an individuals
#' next election.
#' 
# load simple politician data
df_politicians <- read_csv('Data/Politicians_Data/overview.csv')
df_politicians$party_name <- factor(df_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_politicians)[colnames(df_politicians) == 'followers_count'] <- 'followers'

# load politician data by election
df_election_politicians <- read_csv('Data/Politicians_Data/elections_without_governors.csv')
df_election_politicians <- merge(df_politicians, select(df_election_politicians, c(year, first_last, candidatevotes, totalvotes, share, elected, incumbent)), by = 'first_last')
df_election_politicians$party_name <- factor(df_election_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_election_politicians)[colnames(df_election_politicians) == 'followers_count'] <- 'followers'

if (twitter_data_selector == 'all') {
  df_campaign_data_all <- merge(mutate(df_complete_data_all, year = as.numeric(format(next_election_day, '%Y'))), 
                                select(df_election_politicians, c(year, screen_name)), 
                                by = c('year', 'screen_name'))
}
if (twitter_data_selector == 'combined') {
  df_campaign_data_all <- merge(mutate(df_complete_data_all, year = as.numeric(format(next_election_day, '%Y'))), 
                                select(df_election_politicians, c(year, first_last))[!duplicated(select(df_election_politicians, c(year, first_last))),], 
                                by = c('year', 'first_last'))
}
df_campaign_data_all <- select(df_campaign_data_all, -c(year))

df_non_campaign_data_all <- setdiff(df_complete_data_all, df_campaign_data_all)

df_complete_data_all <- rbind(
  mutate(df_campaign_data_all, campaign = TRUE),
  mutate(df_non_campaign_data_all, campaign = FALSE)
)

columns_politicians <- c('first_last', 'office_title', 'party_name', 'state', 
                         'gender', 'followers', 'friends_count', 'listed_count',
                         'favourites_count')

if (twitter_data_selector == 'combined') {
  df_politicians_reduced <- df_politicians %>% 
    group_by(first_name) %>% 
    top_n(1, followers) %>%
    ungroup() %>%
    select(all_of(columns_politicians))
  df_complete_data_all_politicians <- merge(df_complete_data_all,
                                            df_politicians_reduced,
                                            by = 'first_last')
  write_csv(df_complete_data_all_politicians, 'Data/Tweet_Data/Filtered_Data/combined_politicians_extended.csv')
  write_csv(df_complete_data_all, 'Data/Tweet_Data/Filtered_Data/combined.csv')
}
if (twitter_data_selector == 'all') {
  df_politicians_reduced <- df_politicians %>%
    select(all_of(columns_politicians))
  df_complete_data_all_politicians <- merge(df_complete_data_all,
                                            df_politicians_reduced,
                                            by = 'screen_name')
  write_csv(df_complete_data_all_politicians, 'Data/Tweet_Data/Filtered_Data/all_politicians_extended.csv')
  write_csv(df_complete_data_all, 'Data/Tweet_Data/Filtered_Data/all.csv')
}


# Event Data ----
df_events_climate <- read_csv('Data/Event_Data/climate_events.csv', skip = 1)
event_dates_climate <- ymd(df_events_climate$`Begin Date`)

for (col in c('Begin Date', 'End Date')){
  df_events_climate[col] = ymd(df_events_climate[[col]])
}

df_events_gun <- read_csv('Data/Event_Data/mass_shootings.csv')
df_events_gun <- df_events_gun %>%
  filter(killed >= 8)
df_events_gun$date <- mdy(df_events_gun$date)
event_dates_gun <- ymd(df_events_gun$date)


# Load Twitter Data
df_complete_data_all_politicians <- read_csv('Data/Tweet_Data/Filtered_Data/combined_politicians_extended.csv')
df_complete_data_all <- read_csv('Data/Tweet_Data/Filtered_Data/combined.csv')

# Change campaign period to the period one year before an election
df_complete_data_all_politicians <- df_complete_data_all_politicians %>%
  mutate(campaign = ifelse(days_till_election > 170, FALSE, campaign))

df_complete_data_all <- df_complete_data_all %>%
  mutate(campaign = ifelse(days_till_election > 170, FALSE, campaign))

# Filter out wrong data (goal to get the meta data for each Tweet again)
df_complete_data_all <- df_complete_data_all %>%
  filter(favorite_count > 0 | retweet_count < 100)

df_complete_data_all_politicians <- df_complete_data_all_politicians %>%
  filter(favorite_count > 0 | retweet_count < 100)

df_complete_data_all <- df_complete_data_all %>%
  filter(retweet_count < 10000)

# ---- No Additional Politician Information ----
df_complete_data_all <- filter(df_complete_data_all, election_type != 'Governor')
df_complete_climate <- filter(df_complete_data_all, topic == 'climate_change')
df_complete_gun <- filter(df_complete_data_all, topic == 'gun_policy')


# ---- Implied Events ----
if (event_type == 'implied') {
  df_number_daily_tweets_climate <- df_complete_climate %>%
    group_by(created_at) %>%
    summarise(n_tweets = n())
  
  ggplot(df_number_daily_tweets_climate, aes(ymd(created_at), n_tweets)) +
    geom_line()
  
  event_dates_climate <- filter(df_number_daily_tweets_climate, n_tweets > 75)$created_at
  
  df_number_daily_tweets_gun <- df_complete_gun %>%
    group_by(created_at) %>%
    summarise(n_tweets = n())
  
  ggplot(df_number_daily_tweets_gun, aes(ymd(created_at), n_tweets)) +
    geom_line()
  
  event_dates_gun <- filter(df_number_daily_tweets_gun, n_tweets > 100)$created_at
}

# Analysis Climate Change ----
df_regression_all_tweets_climate <- tibble()
df_complete_climate_copy <- df_complete_climate

for (i in seq(length(event_dates_climate), 1)) {
  current_df <- df_complete_climate_copy %>%
    filter(ymd(created_at) >= ymd(event_dates_climate[i])) %>%
    mutate(reaction_time = difftime(ymd(format(created_at, '%Y-%m-%d')), event_dates_climate[i], units = 'days'))
  df_complete_climate_copy <- filter(df_complete_climate_copy, ymd(created_at) < ymd(event_dates_climate[i]))
  df_regression_all_tweets_climate <- rbind(df_regression_all_tweets_climate, current_df)
} 
rm(df_complete_climate_copy)

df_regression_all_tweets_climate <- df_regression_all_tweets_climate %>%
  mutate(year = as.integer(format(created_at, '%Y'))) %>%
  filter(!grepl('RT ', text, fixed = T)) %>%
  mutate(year_month = format(created_at, '%Y-%m'))

df_regression_all_tweets_climate$reaction_time <- as.numeric(df_regression_all_tweets_climate$reaction_time)
df_regression_all_tweets_climate$days_till_election <- as.numeric(df_regression_all_tweets_climate$days_till_election)

reg_reaction_fixed_effects_simple <- plm(reaction_time ~ days_till_election + first_last + election_type*days_till_election +
                                           as.factor(year), 
                                     data = filter(df_regression_all_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                     index = c('first_last'), 
                                     model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(reaction_time ~ days_till_election + campaign*election_type*days_till_election + as.factor(year), 
                                  data = filter(df_regression_all_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

#' Big Question does the days till election effect go through reaction time or is it seperate effect

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                         data = filter(df_regression_all_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                         index = c('first_last'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)
reg_retweets_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                    + campaign*reaction_time + as.factor(year),
                                  data = filter(df_regression_all_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_retweets_fixed_effects)

reg_likes_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                      data = filter(df_regression_all_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_likes_fixed_effects_simple)
reg_likes_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                 + campaign*reaction_time + as.factor(year),
                               data = filter(df_regression_all_tweets_climate, (year <= 2020) & (year >= 2017)), 
                               index = c('first_last'), 
                               model = 'within')
summary(reg_likes_fixed_effects)

# First Tweet Only ----
df_regression_all_tweets_climate_copy <- df_regression_all_tweets_climate
df_regression_first_tweet_climate <- tibble()
for (i in seq(length(event_dates_climate), 1)) {
  current_df <- filter(df_regression_all_tweets_climate_copy, ymd(created_at) >= ymd(event_dates_climate[i]))
  df_regression_all_tweets_climate_copy <- filter(df_regression_all_tweets_climate_copy, ymd(created_at) < ymd(event_dates_climate[i]))
  current_df <- current_df %>%
    group_by(first_last) %>%
    filter(created_at == max(created_at))
  df_regression_first_tweet_climate <- rbind(df_regression_first_tweet_climate, current_df)
}
rm(df_regression_all_tweets_climate_copy)

reg_first_reaction_fixed_effects_simple <- plm(reaction_time ~ days_till_election + first_last +
                                                 as.factor(year), 
                                               data = filter(df_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                               index = c('first_last'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(reaction_time ~ days_till_election + campaign + campaign*days_till_election +
                                          as.factor(year), 
                                        data = filter(df_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

reg_first_retweets_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                               data = filter(df_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                               index = c('first_last'), 
                                               model = 'within')
summary(reg_first_retweets_fixed_effects_simple)
reg_first_retweets_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                          + campaign*reaction_time + as.factor(year),
                                        data = filter(df_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_retweets_fixed_effects)

reg_first_likes_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                            data = filter(df_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_likes_fixed_effects_simple)
reg_first_likes_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                       + campaign*reaction_time + as.factor(year),
                                     data = filter(df_regression_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                     index = c('first_last'), 
                                     model = 'within')
summary(reg_first_likes_fixed_effects)


# Analysis Gun Policy ----
df_regression_all_tweets_gun <- tibble()
df_complete_gun_copy <- df_complete_gun

for (i in seq(length(event_dates_gun), 1)) {
  current_df <- df_complete_gun_copy %>%
    filter(ymd(created_at) >= ymd(event_dates_gun[i])) %>%
    mutate(reaction_time = difftime(ymd(format(created_at, '%Y-%m-%d')), event_dates_gun[i], units = 'days'))
  df_complete_gun_copy <- filter(df_complete_gun_copy, ymd(created_at) < ymd(event_dates_gun[i]))
  df_regression_all_tweets_gun <- rbind(df_regression_all_tweets_gun, current_df)
} 
rm(df_complete_gun_copy)

df_regression_all_tweets_gun <- df_regression_all_tweets_gun %>%
  mutate(year = as.integer(format(created_at, '%Y'))) %>%
  filter(!grepl('RT ', text, fixed = T)) %>%
  mutate(year_month = format(created_at, '%Y-%m'))

df_regression_all_tweets_gun$reaction_time <- as.numeric(df_regression_all_tweets_gun$reaction_time)
df_regression_all_tweets_gun$days_till_election <- as.numeric(df_regression_all_tweets_gun$days_till_election)

reg_reaction_fixed_effects_simple <- plm(reaction_time ~ days_till_election + first_last +
                                           as.factor(year), 
                                         data = df_regression_all_tweets_gun,
                                         index = c('first_last'), 
                                         model = 'within')
summary(reg_reaction_fixed_effects_simple)
reg_reaction_fixed_effects <- plm(reaction_time ~ days_till_election + campaign + campaign*days_till_election +
                                    as.factor(year), 
                                  data = df_regression_all_tweets_gun,
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_reaction_fixed_effects)

reg_retweets_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                         data = df_regression_all_tweets_gun,
                                         index = c('first_last'), 
                                         model = 'within')
summary(reg_retweets_fixed_effects_simple)
reg_retweets_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                    + campaign*reaction_time + as.factor(year),
                                  data = df_regression_all_tweets_gun,
                                  index = c('first_last'), 
                                  model = 'within')
summary(reg_retweets_fixed_effects)

reg_likes_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                      data = df_regression_all_tweets_gun,
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_likes_fixed_effects_simple)
reg_likes_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                 + campaign*reaction_time + as.factor(year),
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

reg_first_reaction_fixed_effects_simple <- plm(reaction_time ~ days_till_election + first_last +
                                                 as.factor(year), 
                                               data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                               index = c('first_last'), 
                                               model = 'within')
summary(reg_first_reaction_fixed_effects_simple)
reg_first_reaction_fixed_effects <- plm(reaction_time ~ days_till_election + campaign + campaign*days_till_election +
                                          as.factor(year), 
                                        data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_reaction_fixed_effects)

reg_first_retweets_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                               data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                               index = c('first_last'), 
                                               model = 'within')
summary(reg_first_retweets_fixed_effects_simple)
reg_first_retweets_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                          + campaign*reaction_time + as.factor(year),
                                        data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                        index = c('first_last'), 
                                        model = 'within')
summary(reg_first_retweets_fixed_effects)

reg_first_likes_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                            data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_likes_fixed_effects_simple)
reg_first_likes_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                       + campaign*reaction_time + as.factor(year),
                                     data = filter(df_regression_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                     index = c('first_last'), 
                                     model = 'within')
summary(reg_first_likes_fixed_effects)



# Graphs ----
df_regression_all_tweets <- rbind(df_regression_all_tweets_climate,
                                  df_regression_all_tweets_gun)


# Graph showing the amount of Tweets to a certain topic n days after an event related to that topic ----
df_reaction_time_distribution <- df_regression_all_tweets %>%
  group_by(reaction_time, topic) %>%
  summarise(n = n()) 

df_total_per_group <- df_politicians %>%
  filter(office_title %in% c('Senator', 'Senate Candidate')) %>%
  group_by(party_name) %>%
  summarise(n_total = n())

df_reaction_time_distribution <- df_reaction_time_distribution %>%
  mutate(share = n / ifelse(party_name == df_total_per_group$party_name[1], 
                            df_total_per_group$n_total[1],
                            df_total_per_group$n_total[2]))

ggplot(df_reaction_time_distribution, aes(reaction_time, share, color = party_name)) +
  geom_point()

ggplot(df_regression_all_tweets, aes(reaction_time)) +
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

reg_first_retweets <- lm(retweet_count ~ reaction_time + year + party_name + followers + followers*year, df_regression_first_tweet)
summary(reg_first_retweets)

reg_first_likes <- lm(favorite_count ~ reaction_time + year + party_name + followers + followers*year, df_regression_first_tweet)
summary(reg_first_likes)




# ---- Additional Politician Information ----
df_complete_data_all_politicians <- filter(df_complete_data_all_politicians, election_type != 'Governor')
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
    mutate(reaction_time = difftime(ymd(format(created_at, '%Y-%m-%d')), event_dates_climate[i], units = 'days'))
  df_complete_pol_information_climate_copy <- filter(df_complete_pol_information_climate_copy, ymd(created_at) < ymd(event_dates_climate[i]))
  df_regression_pol_tweets_climate <- rbind(df_regression_pol_tweets_climate, current_df)
} 
rm(df_complete_pol_information_climate_copy)

df_regression_pol_tweets_climate <- df_regression_pol_tweets_climate %>%
  mutate(year = as.integer(format(created_at, '%Y'))) %>%
  filter(!grepl('RT ', text, fixed = T)) %>%
  mutate(year_month = format(created_at, '%Y-%m'))

df_regression_pol_tweets_climate$reaction_time <- as.numeric(df_regression_pol_tweets_climate$reaction_time)
df_regression_pol_tweets_climate$days_till_election <- as.numeric(df_regression_pol_tweets_climate$days_till_election)

reg_reaction_pol_fixed_effects_simple <- plm(reaction_time ~ days_till_election +
                                               election_type + days_till_election*election_type + as.factor(year), 
                                             data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_reaction_pol_fixed_effects_simple)
reg_reaction_pol_fixed_effects <- plm(reaction_time ~ days_till_election + campaign + campaign*days_till_election +
                                        as.factor(year) + first_last, 
                                      data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_reaction_pol_fixed_effects)

reg_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                             data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_retweets_pol_fixed_effects_simple)
reg_retweets_pol_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                        campaign*reaction_time + as.factor(year),
                                      data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_retweets_pol_fixed_effects)

reg_likes_pol_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                          data = filter(df_regression_pol_tweets_climate, (year <= 2020) & (year >= 2017)), 
                                          index = c('first_last'), 
                                          model = 'within')
summary(reg_likes_pol_fixed_effects_simple)
reg_likes_pol_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                     + campaign*reaction_time + as.factor(year),
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

reg_first_reaction_pol_fixed_effects_simple <- plm(reaction_time ~ days_till_election + first_last +
                                                 as.factor(year), 
                                               data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                               index = c('first_last'), 
                                               model = 'within')
summary(reg_first_reaction_pol_fixed_effects_simple)
reg_first_reaction_pol_fixed_effects <- plm(reaction_time ~ days_till_election + campaign + campaign*days_till_election +
                                              as.factor(year), 
                                            data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_reaction_pol_fixed_effects)

reg_first_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                                   data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                                   index = c('first_last'), 
                                                   model = 'within')
summary(reg_first_retweets_pol_fixed_effects_simple)
reg_first_retweets_pol_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                              + campaign*reaction_time + as.factor(year),
                                            data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_retweets_pol_fixed_effects)

reg_first_likes_pol_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                                data = filter(df_regression_pol_first_tweet_climate, (year <= 2020) & (year >= 2017)), 
                                                index = c('first_last'), 
                                                model = 'within')
summary(reg_first_likes_pol_fixed_effects_simple)
reg_first_likes_pol_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                           + campaign*reaction_time + as.factor(year),
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
    mutate(reaction_time = difftime(ymd(format(created_at, '%Y-%m-%d')), event_dates_gun[i], units = 'days'))
  df_complete_pol_information_gun_copy <- filter(df_complete_pol_information_gun_copy, ymd(created_at) < ymd(event_dates_gun[i]))
  df_regression_pol_tweets_gun <- rbind(df_regression_pol_tweets_gun, current_df)
} 
rm(df_complete_pol_information_gun_copy)

df_regression_pol_tweets_gun <- df_regression_pol_tweets_gun %>%
  mutate(year = as.integer(format(created_at, '%Y'))) %>%
  filter(!grepl('RT ', text, fixed = T)) %>%
  mutate(year_month = format(created_at, '%Y-%m'))

df_regression_pol_tweets_gun$reaction_time <- as.numeric(df_regression_pol_tweets_gun$reaction_time)
df_regression_pol_tweets_gun$days_till_election <- as.numeric(df_regression_pol_tweets_gun$days_till_election)

reg_reaction_pol_fixed_effects_simple <- plm(reaction_time ~ days_till_election + first_last +
                                               as.factor(year), 
                                             data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_reaction_pol_fixed_effects_simple)
reg_reaction_pol_fixed_effects <- plm(reaction_time ~ days_till_election + campaign + campaign*days_till_election +
                                        as.factor(year), 
                                      data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_reaction_pol_fixed_effects)

summary(lm(retweet_count ~ reaction_time + days_till_election + as.factor(year), filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017))))
reg_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                             data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                             index = c('first_last'), 
                                             model = 'within')
summary(reg_retweets_pol_fixed_effects_simple)
reg_retweets_pol_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                        reaction_time*party_name + election_type + as.factor(year),
                                      data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                      index = c('first_last'), 
                                      model = 'within')
summary(reg_retweets_pol_fixed_effects)
summary(lm(retweet_count ~ reaction_time + days_till_election + party_name + reaction_time*party_name + as.factor(year), filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017))))

reg_likes_pol_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                          data = filter(df_regression_pol_tweets_gun, (year <= 2020) & (year >= 2017)), 
                                          index = c('first_last'), 
                                          model = 'within')
summary(reg_likes_pol_fixed_effects_simple)
reg_likes_pol_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                     + campaign*reaction_time + as.factor(year),
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
df_reaction_time_distribution <- df_regression_all_tweets %>%
  group_by(party_name, reaction_time) %>%
  summarise(n = n()) 

df_total_per_group <- df_politicians %>%
  filter(office_title %in% c('Senator', 'Senate Candidate')) %>%
  group_by(party_name) %>%
  summarise(n_total = n())

df_reaction_time_distribution <- df_reaction_time_distribution %>%
  mutate(share = n / ifelse(party_name == df_total_per_group$party_name[1], 
                            df_total_per_group$n_total[1],
                            df_total_per_group$n_total[2]))

ggplot(df_reaction_time_distribution, aes(reaction_time, share, color = party_name)) +
  geom_point()

ggplot(df_regression_all_tweets, aes(reaction_time)) +
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

summary(lm(reaction_time ~ days_till_election + as.factor(year), filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017))))
reg_first_reaction_pol_fixed_effects_simple <- plm(reaction_time ~ days_till_election +
                                                     as.factor(year), 
                                                   data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                                   index = c('first_last'), 
                                                   model = 'within')
summary(reg_first_reaction_pol_fixed_effects_simple)
reg_first_reaction_pol_fixed_effects <- plm(reaction_time ~ days_till_election + campaign + campaign*days_till_election +
                                              as.factor(year), 
                                            data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_reaction_pol_fixed_effects)

summary(lm(retweet_count ~ reaction_time + days_till_election + as.factor(year), filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017))))
reg_first_retweets_pol_fixed_effects_simple <- plm(retweet_count ~ reaction_time + days_till_election + as.factor(year),
                                                   data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                                   index = c('first_last'), 
                                                   model = 'within')
summary(reg_first_retweets_pol_fixed_effects_simple)
reg_first_retweets_pol_fixed_effects <- plm(retweet_count ~ reaction_time + days_till_election + campaign + 
                                              + campaign*reaction_time + as.factor(year),
                                            data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                            index = c('first_last'), 
                                            model = 'within')
summary(reg_first_retweets_pol_fixed_effects)

reg_first_likes_pol_fixed_effects_simple <- plm(favorite_count ~ reaction_time + days_till_election + as.factor(year),
                                                data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                                index = c('first_last'), 
                                                model = 'within')
summary(reg_first_likes_pol_fixed_effects_simple)
reg_first_likes_pol_fixed_effects <- plm(favorite_count ~ reaction_time + days_till_election + campaign + 
                                           + campaign*reaction_time + as.factor(year),
                                         data = filter(df_regression_pol_first_tweet_gun, (year <= 2020) & (year >= 2017)), 
                                         index = c('first_last'), 
                                         model = 'within')
summary(reg_first_likes_pol_fixed_effects)




