#' The purpose of this script is to get one big data frame with all the necessary data
#' to execute all analysis in this paper. The following columns are required:
#' created_at: the date on which the Tweet was posted
#' year_month: time of posting at month level
#' year: time of posing at year level
#' first_last: first and last name of politicians to have uniform identification
#' topic: which of the three topics is covered in the Tweet (climate, gun, or immigration policy)
#' office_type: the type of office politicians are running for (house, senate)
#' party_name: the party to which the politicans belong (Democrat, Republican)
#' state: the state for which they run
#' text: the content of the Tweet
#' like_count: the number of likes the Tweet received
#' retweet_count: the number of retweets the Tweet receive
#' pre_delta_days: number of days between a Tweet and the next event within the same topic
#' post_delta_days: number of days between a Tweet and the last event within the same topic
#' implied_pre_delta_days: number of days between a Tweet and the next implied event within the same topic
#' implied_post_delta_days: number of days between a Tweet and the last implied event within the same topic
#' days_till_election: number of days between the last event before the Tweet and the next election

library(tidyverse)
library(Microsoft365R)
library(lubridate)

rm(list=ls())

source("helper_functions.R")
od_drive <- get_business_onedrive()

topics <- c('climate_change', 'gun_laws', 'immigration')

twitter_data_selector <- 'all'

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

# read all Tweet data by politician and save filtered version within one big data frame
df_complete_data_all <- read_all_files_filtered_to_df(od_drive, tweet_dir_data_paths_od, topics, twitter_data_selector)

df_immigration <- df_complete_data_all %>%
  filter(topic == 'immigration')

df_climate <- df_complete_data_all %>%
  filter(topic == 'climate_change')

df_guns <- df_complete_data_all %>%
  filter(topic == 'gun_policy')

### Event Data ----
df_events_climate <- read_csv('Data/Event_Data/climate_events.csv', skip = 1)
df_events_climate$`Begin Date` <- ymd(df_events_climate$`Begin Date`)
df_events_climate$`End Date` <- ymd(df_events_climate$`End Date`)
df_events_climate$`Total CPI-Adjusted Cost (Millions of Dollars)` <- as.numeric(df_events_climate$`Total CPI-Adjusted Cost (Millions of Dollars)`)
df_events_climate <- df_events_climate %>%
  mutate(duration = `End Date` - `Begin Date`) %>%
  filter(duration <= duration(30, 'days')) %>%
  filter(`Total CPI-Adjusted Cost (Millions of Dollars)` > 5000 | Deaths >= 10)
event_dates_climate <- ymd(df_events_climate$`Begin Date`)

df_events_gun <- read_csv('Data/Event_Data/mass_shootings.csv')
df_events_gun <- df_events_gun %>%
  filter(killed >= 8)
df_events_gun$date <- mdy(df_events_gun$date)
event_dates_gun <- ymd(df_events_gun$date)

events_immigration <- c('DACA announcement', 'Senate pasees Immigration Reform Bill', 'Shooting of Kate Steinle', 'Shooter of Kate Steinle acquitted of all charges',
                        'Rockville High School Rape Case', 'Iowa Mollie Tibbetts Case Murder Identification', 'Boston Marathon Bombing Suspects Identification',
                        'Vaughan Foods Beheading Incident', 'Chattanooga shootings', 'San Bernardino Attack', 'Ohio Restaurant Machete Attack',
                        'Orlando Nightclub Shooting', 'Ohio State University Attack', 'New York City Truck Attack', 'Naval Air Station Pensacola shooting')
event_dates_immigration <- c(ymd('2012-06-15'), ymd('2013-06-27'), ymd('2015-07-01'), ymd('2017-11-30'),
                             ymd('2017-03-16'),  ymd('2018-08-22'), ymd('2013-04-18'),
                             ymd('2014-09-25'), ymd('2015-07-16'), ymd('2015-12-02'), ymd('2016-02-11'),
                             ymd('2016-06-12'), ymd('2016-11-28'), ymd('2017-10-31'), ymd('2019-12-06'))
event_dates_immigration <- sort(event_dates_immigration)


### Implied Event Dates ----
#' Non-linear threshold
#' - deal with increase number of Tweets
lin_threshold <- function(x, slope, intercept) {
  slope * x + intercept
}
# 
# for (i_topic in unique(df_complete_data_all$topic)) {
#   print(i_topic)
#   df_tweets_by_day <- df_complete_data_all %>%
#     filter(topic == i_topic) %>%
#     group_by(created_at) %>%
#     summarise(n = n())
#   
  # ggplot(df_tweets_by_day, aes(created_at, n)) +
  #   geom_line()
#   
#   if (i_topic == 'climate_change') {
#     df_sumbsample_climate <- filter(df_tweets_by_day, created_at >= ymd('2012-07-30'))
#     # Set the threshold slope and intercept values
#     slope <- 0.03471329
#     intercept <- -524.8611
#     implied_event_dates_climate <- df_sumbsample_climate[df_sumbsample_climate$n >= lin_threshold(as.numeric(df_sumbsample_climate$created_at), slope, intercept), ]$created_at
#     # only keep first date of subsequent days
#     implied_event_dates_climate <- implied_event_dates_climate[append(c(2), diff(implied_event_dates_climate)) > 1]
#     
#     plot_selection <- ggplot(df_tweets_by_day, aes(created_at, n)) +
#       geom_point() +
#       geom_abline(intercept = intercept, slope = slope)
#     print(plot_selection)
#   }
#   if (i_topic == 'gun_policy') {
#     df_sumbsample_gun <- filter(df_tweets_by_day, created_at >= ymd('2012-02-01'))
#     # Set the threshold slope and intercept values
#     slope <- 0.0186629526462
#     intercept <- -237.0986072423398
#     implied_event_dates_gun <- df_sumbsample_gun[df_sumbsample_gun$n >= lin_threshold(as.numeric(df_sumbsample_gun$created_at), slope, intercept), ]$created_at
#     # only keep first date of subsequent days
#     implied_event_dates_gun <- implied_event_dates_gun[append(c(2), diff(implied_event_dates_gun)) > 1]
#     
#     plot_selection <- ggplot(df_tweets_by_day, aes(created_at, n)) +
#       geom_point() +
#       geom_abline(intercept = intercept, slope = slope)
#     print(plot_selection)
#   }
#   if (i_topic == 'immigration') {
#     df_sumbsample_immigration <- filter(df_tweets_by_day, created_at >= ymd('2012-06-01'))
#     # Set the threshold slope and intercept values
#     slope <- 0.0547645125958
#     intercept <- -799.178532311
#     implied_event_dates_immigration <- df_sumbsample_immigration[df_sumbsample_immigration$n >= lin_threshold(as.numeric(df_sumbsample_immigration$created_at), slope, intercept), ]$created_at
#     # only keep first date of subsequent days
#     implied_event_dates_immigration <- implied_event_dates_immigration[append(c(2), diff(implied_event_dates_immigration)) > 1]
#     
#     plot_selection <- ggplot(df_tweets_by_day, aes(created_at, n)) +
#       geom_point() +
#       geom_abline(intercept = intercept, slope = slope)
#     print(plot_selection)
#   }
# }

#' Implied Event Dates By Change
for (i_topic in unique(df_complete_data_all$topic)) {
  print(i_topic)
  df_tweets_by_day <- df_complete_data_all %>%
    filter(topic == i_topic) %>%
    group_by(created_at) %>%
    summarise(n = n()) %>%
    mutate(change_multiple = n / dplyr::lag(n))

  ggplot(df_tweets_by_day, aes(created_at, n)) +
    geom_line()

  if (i_topic == 'climate_change') {
    df_sumbsample_climate <- filter(df_tweets_by_day, created_at >= ymd('2012-07-30'))
    # Set the threshold slope and intercept values
    slope <- 0.03471329
    intercept <- -524.8611
    implied_event_dates_climate <- df_sumbsample_climate[df_sumbsample_climate$n >= lin_threshold(as.numeric(df_sumbsample_climate$created_at), slope, intercept), ]$created_at
    # only keep first date of subsequent days
    implied_event_dates_climate <- implied_event_dates_climate[append(c(2), diff(implied_event_dates_climate)) > 1]

    plot_selection <- ggplot(df_tweets_by_day, aes(created_at, n)) +
      geom_point() +
      geom_abline(intercept = intercept, slope = slope)
    print(plot_selection)
  }
  if (i_topic == 'gun_policy') {
    df_sumbsample_gun <- filter(df_tweets_by_day, created_at >= ymd('2012-02-01'))
    # Set the threshold slope and intercept values
    slope <- 0.0186629526462
    intercept <- -237.0986072423398
    implied_event_dates_gun <- df_sumbsample_gun[df_sumbsample_gun$n >= lin_threshold(as.numeric(df_sumbsample_gun$created_at), slope, intercept), ]$created_at
    # only keep first date of subsequent days
    implied_event_dates_gun <- implied_event_dates_gun[append(c(2), diff(implied_event_dates_gun)) > 1]

    plot_selection <- ggplot(df_tweets_by_day, aes(created_at, n)) +
      geom_point() +
      geom_abline(intercept = intercept, slope = slope)
    print(plot_selection)
  }
  if (i_topic == 'immigration') {
    df_sumbsample_immigration <- filter(df_tweets_by_day, created_at >= ymd('2012-06-01'))
    # Set the threshold slope and intercept values
    slope <- 0.0547645125958
    intercept <- -799.178532311
    implied_event_dates_immigration <- df_sumbsample_immigration[df_sumbsample_immigration$n >= lin_threshold(as.numeric(df_sumbsample_immigration$created_at), slope, intercept), ]$created_at
    # only keep first date of subsequent days
    implied_event_dates_immigration <- implied_event_dates_immigration[append(c(2), diff(implied_event_dates_immigration)) > 1]

    plot_selection <- ggplot(df_tweets_by_day, aes(created_at, n)) +
      geom_point() +
      geom_abline(intercept = intercept, slope = slope)
    print(plot_selection)
  }
}


### Add Tweet Context Data ----
df_climate <- df_climate %>%
  mutate(pre_delta_days = get_distance_to_next_event(created_at, event_dates_climate)) %>%
  mutate(post_delta_days = get_distance_from_past_event(created_at, event_dates_climate)) %>%
  mutate(implied_pre_delta_days = get_distance_to_next_event(created_at, implied_event_dates_climate)) %>%
  mutate(implied_post_delta_days = get_distance_from_past_event(created_at, implied_event_dates_climate)) %>%
  mutate(days_till_election = get_distance_to_next_election(created_at) + post_delta_days) %>%
  mutate(days_till_election = ifelse(days_till_election <= 730, days_till_election, days_till_election - 730))
cor(as.numeric(df_climate$pre_delta_days), as.numeric(df_climate$implied_pre_delta_days), use = 'complete.obs')

df_guns <- df_guns %>%
  mutate(pre_delta_days = get_distance_to_next_event(created_at, event_dates_gun)) %>%
  mutate(post_delta_days = get_distance_from_past_event(created_at, event_dates_gun)) %>%
  mutate(implied_pre_delta_days = get_distance_to_next_event(created_at, implied_event_dates_gun)) %>%
  mutate(implied_post_delta_days = get_distance_from_past_event(created_at, implied_event_dates_gun)) %>%
  mutate(days_till_election = get_distance_to_next_election(created_at) + post_delta_days) %>%
  mutate(days_till_election = ifelse(days_till_election <= 730, days_till_election, days_till_election - 730))

df_immigration <- df_immigration %>%
  mutate(pre_delta_days = get_distance_to_next_event(created_at, event_dates_immigration)) %>%
  mutate(post_delta_days = get_distance_from_past_event(created_at, event_dates_immigration)) %>%
  mutate(implied_pre_delta_days = get_distance_to_next_event(created_at, implied_event_dates_immigration)) %>%
  mutate(implied_post_delta_days = get_distance_from_past_event(created_at, implied_event_dates_immigration)) %>%
  mutate(days_till_election = get_distance_to_next_election(created_at) + post_delta_days) %>%
  mutate(days_till_election = ifelse(days_till_election <= 730, days_till_election, days_till_election - 730))

df_complete_processed <- rbind(df_climate, df_guns, df_immigration)


### Election Data ----
# load simple politician data
df_politicians <- read_csv('Data/Politicians_Data/overview.csv')
df_politicians$party_name <- factor(df_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_politicians)[colnames(df_politicians) == 'followers_count'] <- 'followers'
df_politicians <- df_politicians[order(df_politicians$followers, decreasing = TRUE),]
df_politicians_reduced <- df_politicians[!duplicated(df_politicians$first_last),]
df_politicians_selected <- df_politicians_reduced %>%
  select(c('first_last', 'office_title', 'party_name', 'state', 'gender', 'name', 'followers', 'friends_count', 'listed_count'))

# load politician data by election
df_election_politicians <- read_csv('Data/Politicians_Data/elections_without_governors.csv')
df_election_politicians <- merge(df_politicians, select(df_election_politicians, c(year, first_last, candidatevotes, totalvotes, share, elected, incumbent)), by = 'first_last')
df_election_politicians$party_name <- factor(df_election_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_election_politicians)[colnames(df_election_politicians) == 'followers_count'] <- 'followers'
df_election_politicians_selected <- select(df_election_politicians, c('first_last', 'year', 'candidatevotes', 'totalvotes', 'share', 'elected', 'incumbent'))


### Process Data ----
# create simple data set
df_data_simple <- merge(df_complete_processed, df_politicians_selected, by = 'scree') %>%
  mutate(year_month = format(created_at, '%Y-%m')) %>%
  mutate(year = format(created_at, '%Y')) %>%
  mutate(campaign_period = ifelse(days_till_election <= 120, TRUE, FALSE))

# created extensive data set
# add campaign data
df_campaign <- df_data_simple %>%
  mutate(campaign_period = ifelse(days_till_election <= 120, TRUE, FALSE)) %>%
  mutate(next_election_day = get_next_election_day(created_at)) %>%
  mutate(in_upcoming_election = ifelse(format(next_election_day, '%Y') %in% filter(df_election_politicians_selected, first_last == first_last)$year, TRUE, FALSE))

df_election_politicians_selected <- df_election_politicians_selected %>%
  mutate(election_day = get_election_day(year))

df_extensive_time_cutoff <- df_campaign %>%
  filter(election_type != 'Governor') %>%
  filter(created_at < ymd('2020-11-03'))

list_extensive_grouped <- split(df_extensive_time_cutoff, df_extensive_time_cutoff$first_last)

df_incumbent_extensive <- tibble()
all_first_last <- names(list_extensive_grouped)

for (i in 1:length(all_first_last)) {
  df <- list_extensive_grouped[[all_first_last[i]]]
  incumbent_elections <- filter(df_election_politicians_selected, first_last == all_first_last[i] & incumbent == TRUE)$election_day
  if (length(incumbent_elections) >= 1){
    incumbent_days <- c()
    for (election_day in incumbent_elections) {
      election_day <- as.Date(election_day, origin = '1970-01-01')
      term_length <- ifelse(df$election_type[1] == 'House', 2, 6)
      incumbent_days <- append(incumbent_days, seq(get_election_day(format(election_day - years(term_length), '%Y')) + days(1), election_day, by = 'day'))
    }      
    df <- df %>%
      mutate(incumbent = ifelse(created_at %in% incumbent_days, TRUE, FALSE))
  } else {
    df <- df %>%
      mutate(incumbent = FALSE)
  }
  df_incumbent_extensive <- rbind(df_incumbent_extensive, df) 
}

df_data_simple <- filter(df_data_simple, party_name != 'Libertarian')
df_incumbent_extensive <- df_incumbent_extensive %>%
  filter(party_name != 'Libertarian') %>%
  mutate(campaign_mode = as.logical(campaign_period * in_upcoming_election))

write_csv(df_data_simple, 'Data/Processed/simple.csv')
write_csv(df_incumbent_extensive, 'Data/Processed/extensive.csv')

ggplot() +
  geom_histogram(aes(unique(df_guns$days_till_election)))
