# rm(list=ls())

library(tidyverse)
library(Microsoft365R)
library(lubridate)
library(sandwich)
library(stargazer)
library(plm)

source("helper_functions.R")

od_drive <- get_business_onedrive()

# ---- Set Up ----
# Parameter Selection ----
type <- 'house'
#' options:
#' - all: every single Twitter account
#' - combined: combined Twitter accounts for each politician (for each months the
#'             Tweets of the more active account are taken)
twitter_data_selector <- 'combined'

# chose the topic for current analysis
topic <- 'gun_violence'

# potential additional topics: immigration
topics <- c('climate_change', 'gun_laws')


# Get Data Paths and Set Selectors ----
if (type == 'senate') {
  tweet_dir_data_path_od <- ifelse(twitter_data_selector == 'all', 
                                   "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate/", 
                                   "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate_Single/")
  data_gaps_path <- ifelse(twitter_data_selector == 'all', 
                           'Data/Data_Gaps/senate_all_gaps.RDS',
                           'Data/Data_Gaps/senate_all_gaps_combined.RDS')
  account_info_path <- ifelse(twitter_data_selector == 'all', 
                              'Data/Data_Gaps/senate_gap_overview.csv',
                              'Data/Data_Gaps/senate_gap_overview_combined.csv')
}

if (type == 'house') {
  tweet_dir_data_path_od <- ifelse(twitter_data_selector == 'all', 
                                   "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House/",
                                   "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House_Single/")
  data_gaps_path <- ifelse(twitter_data_selector == 'all', 
                           'Data/Data_Gaps/house_all_gaps.RDS',
                           'Data/Data_Gaps/house_all_gaps_combined.RDS')
  account_info_path <- ifelse(twitter_data_selector == 'all', 
                              'Data/Data_Gaps/house_gap_overview.csv',
                              'Data/Data_Gaps/house_gap_overview_combined.csv')
}

if (type == 'governor') {
  tweet_dir_data_path_od <- ifelse(twitter_data_selector == 'all', 
                                   "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor/",
                                   "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor_Single/")
  data_gaps_path <- ifelse(twitter_data_selector == 'all', 
                           'Data/Data_Gaps/governor_all_gaps.RDS',
                           'Data/Data_Gaps/governor_all_gaps_combined.RDS')
  account_info_path <- ifelse(twitter_data_selector == 'all', 
                              'Data/Data_Gaps/governor_gap_overview.csv',
                              'Data/Data_Gaps/governor_gap_overview_combined.csv')
}

if (type == 'all') { # not completed yet
  tweet_dir_data_path_od <- "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House/"
}

list_selector <- ifelse(topic == 'gun_violence', "gun_laws",
                        ifelse(topic == 'climate_change', "climate_change",
                               ifelse(topic == 'capitol_riots', "january-6",
                                      "covid")))


# Load Politician Data ----
# load simple politician data
df_politicians <- read_csv('Data/Politicians_Data/overview.csv')
df_politicians$party_name <- factor(df_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_politicians)[colnames(df_politicians) == 'followers_count'] <- 'followers'

# load politician data by election
df_election_politicians <- read_csv('Data/Politicians_Data/elections_without_governors.csv')
df_election_politicians <- merge(df_politicians, select(df_election_politicians, c(first_last, candidatevotes, totalvotes, share, elected, incumbent)), by = 'first_last')
df_election_politicians$party_name <- factor(df_election_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_election_politicians)[colnames(df_election_politicians) == 'followers_count'] <- 'followers'


# Load Twitter Data ----
# get Twitter data for selected topics + account summary data frame
df_tweets_granular <- read_all_files_filtered(od_drive, tweet_dir_data_path_od, topics, twitter_data_selector)

# Load Event Data ----
if (topic == 'climate_change') {
  df_events <- read_csv('Data/Event_Data/climate_events.csv', skip = 1)
  event_dates <- ymd(df_events$`Begin Date`)
  
  for (col in c('Begin Date', 'End Date')){
    df_events[col] = ymd(df_events[[col]])
  }
  
  # define month for Twitter analysis, if event happened in the last 5/6 of month include next month
  df_events <- df_events %>%
    mutate(duration = `End Date` - `Begin Date`) %>%
    filter(duration <= duration(30, 'days')) %>%
    mutate(month = ifelse(as.numeric(day(`Begin Date`)) < 25, month(`Begin Date`), month(add_with_rollback(`Begin Date`, months(1))))) %>%
    mutate(year = ifelse(as.numeric(day(`Begin Date`)) > 25 & as.numeric(month(`Begin Date`) == 12), year(add_with_rollback(`Begin Date`, years(1))), year(`Begin Date`))) %>%
    mutate(year_month = format(as.Date(paste(as.character(year), ifelse(month < 10, paste('0', as.character(month), sep = ''), as.character(month)), '01', sep = '-'), '%Y-%m-%d'), '%Y-%m'))
  
  event_months <- union(format(df_events$`Begin Date`, '%Y-%m'), unique(df_events$year_month))
  
  binwidth_number <- .5
  binwidth_share <- .01
}

if (topic == 'gun_violence') {
  df_events <- read_csv('Data/Event_Data/mass_shootings.csv')
  df_events <- df_events %>%
    filter(killed >= 8)
  df_events$date <- mdy(df_events$date)
  event_dates <- ymd(df_events$date)
  
  df_events <- df_events %>%
    mutate(month = ifelse(as.numeric(day(date)) < 25, month(date), month(add_with_rollback(date, months(1))))) %>%
    mutate(year = ifelse(as.numeric(day(date)) > 25 & as.numeric(month(date) == 12), year(add_with_rollback(date, years(1))), year(date))) %>%
    mutate(year_month = format(as.Date(paste(as.character(year), ifelse(month < 10, paste('0', as.character(month), sep = ''), as.character(month)), '01', sep = '-'), '%Y-%m-%d'), '%Y-%m'))
  
  event_months <- union(format(df_events$date, '%Y-%m'), unique(df_events$year_month))
  
  binwidth_number <- .5
  binwidth_share <- .01
}

if (topic == 'covid') {
  evnet_df <- read_csv('Data/Event_Data/covid.csv')
  event_dates <- ymd(df_events$`Begin Date`)
}

if (topic == 'capitol_riots') {
  event_dates <- ymd_hm('2021-01-06 10:30')
}


# Load Account Information Data ----
# load gap data
data_gaps <- readRDS(data_gaps_path)

# load account info overview including first Tweet
df_account_info <- read_csv(account_info_path)


# ---- Data Summary ----
# Tweets ----
df_total_tweets_per_month <- df_tweets_granular[['activity']] %>%
  group_by(year_month) %>%
  summarise(total_tweets = sum(n)) %>%
  mutate(date = ymd(paste(year_month, '-01', sep=''))) %>%
  mutate(twitter_data = twitter_data_selector) %>%
  mutate(office_type = type)


# Accounts ----
df_total_accounts_per_month <- df_tweets_granular[['activity']] %>%
  group_by(year_month) %>%
  summarise(total_accounts = length(unique(file_name))) %>%
  mutate(date = ymd(paste(year_month, '-01', sep=''))) %>%
  mutate(twitter_data = twitter_data_selector) %>%
  mutate(office_type = type)

total_accounts <- length(unique(df_tweets_granular[['activity']]$file_name))

# Politicians ----


# ---- Twitter Activity Analysis ----
# Data Preparation ----
df_topic_aggregate <- get_aggregate_df(df_tweets_granular[[list_selector]], df_tweets_granular[['activity']], twitter_data_selector)

# get the standard deviations of the Tweets and topic share per user
sd_climate_change <- get_sd_topic(df_topic_aggregate, twitter_data_selector)

# add event column to data frame which is true if it is an event months and False if not
df_event_extended <- df_topic_aggregate %>%
  mutate(event = ifelse(year_month %in% event_months, T, F)) %>%
  mutate(share = n_topic/n)

# Analysis ----
# plot distribution of the standard deviation of number of Tweets
ggplot(sd_climate_change, aes(x=sd_tweets)) +
  geom_histogram(binwidth = binwidth_number)

# plot distribution of the standard deviation of share
ggplot(sd_climate_change, aes(x=sd_share)) +
  geom_histogram(binwidth = binwidth_share)

# chose only observations in 2016 or later
# event_extended_df <- event_extended_df[grepl('2016|2017|2018|2019|2020|2021|2022', event_extended_df$year_month),]

# more Tweets in event months in general
df_event_extended %>%
  group_by(year_month, event) %>%
  summarise(n_topic = sum(n_topic)) %>%
  group_by(event) %>%
  summarise(average = mean(n_topic))

# more Tweets in event months by user
if (twitter_data_selector == 'all') {
  df_event_extended %>%
    group_by(screen_name, event) %>%
    summarise(average = sum(n_topic)/n())
}

if (twitter_data_selector == 'combined') {
  df_event_extended %>%
    group_by(first_last, event) %>%
    summarise(average = sum(n_topic)/n())
}


# same analysis but with Tweet shares
# higher share of tweets in event months aggregated over all users
df_event_extended %>%
  group_by(year_month, event) %>%
  summarise(share = mean(share)) %>%
  group_by(event) %>%
  summarise(average = mean(share))

#' Look at individuals and their distribution to show that there
#' are outlieres months where suddenly they tweet a lot related to this topic



# Covid Analysis not sure if right (early unfinished work)
# list.files(path="Twitter_Sen", pattern="*.csv", full.names=TRUE, recursive=FALSE)

candidates <- c("connieljohnson", "JoshCMandel", "paulmrand", "abbybroyles", "adamhudson5", "adamslily")

covid_filtered <- filter(df_tweets_granular[["covid"]], screen_name %in% candidates)
covid_filterd_count = aggregate_tweet_count(covid_filtered)
merged_covdi_filterd <- merge(covid_filterd_count,df_tweets_granular[["activity"]],by=c("year_month","screen_name"))
merged_covdi_filterd <- merged_covdi_filterd %>%
  mutate(share = n_topic/n)
merged_covdi_filterd$year_month <- as.Date(paste(merged_covdi_filterd$year_month, "-01", sep=""), "%Y-%m-%d")

merged_covdi_filterd <- merged_covdi_filterd %>%
  filter(share < 0.5) %>%
  filter(year_month > as.Date("2019-06-01"))

ggplot(merged_covdi_filterd, aes(x=year_month, y=share, color=screen_name)) +
  geom_line()


# how where tweets before December 2019 already classified as relating to Covid
miss_classification_df <- filter(df_tweets_granular[["covid"]], created_at < as.Date("2019-12-01"))



# ---- Analyze Reaction Time Aggregated ----
## average time of reaction to an event
df_detailed <- df_tweets_granular[[list_selector]]
df_detailed$created_at <- ymd(df_detailed$created_at)

reaction_df <- tibble()
if (twitter_data_selector == 'all') {
  for (name in unique(df_detailed$screen_name)){
    df_individual <- df_detailed %>%
      filter(screen_name == name)
    df_individual <- df_individual[order(df_individual$created_at),]
    durations_individual = c()
    
    # individualize event data based on data gaps and first Tweets
    dates_individual <- event_dates[!format(event_dates, '%Y-%m') %in% data_gaps[[name]][['data_gaps']]]
    first_tweet <- as.Date(filter(df_account_info, screen_name == name)$first_tweet)
    dates_individual <- dates_individual[dates_individual >= first_tweet]
    
    for (i in 1:length(dates_individual)) {
      temp_duration = -1
      try(temp_duration <- as.integer(filter(df_individual, created_at >= dates_individual[i])$created_at[1] - dates_individual[i]))
      if (temp_duration == -1 || is.na(temp_duration)) {
        temp_duration = min(c(300, as.integer(ymd('2022-12-31') - dates_individual[i])))
      }
      durations_individual <- append(durations_individual, temp_duration)
    }
    reaction_df <- rbind(reaction_df, tibble('screen_name' = c(name),
                                             'mean_reaction_in_days' = c(mean(durations_individual))))
  }
}

if (twitter_data_selector == 'combined') {
  for (name in unique(df_detailed$first_last)){
    df_individual <- df_detailed %>%
      filter(first_last == name)
    df_individual <- df_individual[order(df_individual$created_at),]
    durations_individual = c()
    
    # individualize event data based on data gaps and first Tweets
    dates_individual <- event_dates[!format(event_dates, '%Y-%m') %in% data_gaps[[name]][['data_gaps']]]
    first_tweet <- as.Date(filter(df_account_info, first_last == name)$first_tweet)
    dates_individual <- dates_individual[dates_individual >= first_tweet]
    
    for (i in 1:length(dates_individual)) {
      temp_duration = -1
      try(temp_duration <- as.integer(filter(df_individual, created_at >= dates_individual[i])$created_at[1] - dates_individual[i]))
      if (temp_duration == -1 || is.na(temp_duration)) {
        temp_duration = min(c(300, as.integer(ymd('2022-12-31') - dates_individual[i])))
      }
      durations_individual <- append(durations_individual, temp_duration)
    }
    reaction_df <- rbind(reaction_df, tibble('first_last' = c(name),
                                             'mean_reaction_in_days' = c(mean(durations_individual))))
  }
}

df_reaction_extended <- merge(reaction_df, df_politicians[!duplicated(df_politicians$first_last),], by = c('first_last'))

ggplot(df_reaction_extended, aes(x=mean_reaction_in_days, fill = party_name)) +
  geom_histogram(position = 'stack', binwidth = 10)


# ---- Analyze Reaction Time Aggregated (Gun Violence) ----
# Average time of reaction to an event (mass shootings) ----
gun_data <- read_csv('Data/Event_Data/mass_shootings.csv')
gun_data <- gun_data %>%
  filter(killed >= 5 & injured >= 3)
gun_data$date = mdy(gun_data$date)

df_detailed_guns <- df_tweets_granular[['gun_laws']]
df_detailed_guns$created_at <- ymd(df_detailed_guns$created_at)

dates <- ymd(gun_data$date)

reaction_df_guns <- tibble()
for (name in unique(df_detailed_guns$screen_name)){
  df_individual <- df_detailed_guns %>%
    filter(screen_name == name)
  df_individual <- df_individual[order(df_individual$created_at),]
  durations_individual = c()
  
  # individualize event data based on data gaps and first Tweets
  dates_individual <- dates[!format(dates, '%Y-%m') %in% data_gaps[[name]][['data_gaps']]]
  first_tweet <- as.Date(filter(df_account_info, screen_name == name)$first_tweet)
  dates_individual <- dates_individual[dates_individual >= first_tweet]
  
  for (i in 1:length(dates_individual)) {
    temp_duration = -1
    try(temp_duration <- as.integer(filter(df_individual, created_at >= dates_individual[i])$created_at[1] - dates_individual[i]))
    if (temp_duration == -1 || is.na(temp_duration)) {
      temp_duration = min(c(300, as.integer(ymd('2023-01-31') - dates_individual[i])))
    }
    durations_individual <- append(durations_individual, temp_duration)
  }
  reaction_df_guns <- rbind(reaction_df_guns, tibble('screen_name' = c(name),
                              'mean_reaction_in_days' = c(mean(durations_individual))))
}

ggplot(reaction_df_guns, aes(x=mean_reaction_in_days)) +
  geom_histogram(binwidth = 1)


# Analyze if elected politicians deviate from non-elected politicians ----
df_twitter_handles <- read_csv('Data/Twitter_Handle_Data/active_accounts.csv')
colnames(df_twitter_handles)[1] <- 'screen_name'
extended_gun_reaction_df <- merge(df_twitter_handles, reaction_df_guns, by = 'screen_name') %>%
  mutate(winner = ifelse(office_title == 'House Representative', TRUE, ifelse(office_title == 'House Candidate', FALSE, NA)))

ggplot(extended_gun_reaction_df, aes(x=mean_reaction_in_days, color=winner)) +
  geom_histogram(binwidth = 1)

extended_gun_reaction_df %>%
  group_by(winner) %>%
  summarise(mean = mean(mean_reaction_in_days))


# Get reaction time by election period (2-year periods for House) ----
election_dates <- ymd(c('2012-11-06', '2014-11-04', '2016-11-08', '2018-11-06', '2020-11-03', '2022-11-08'))
reaction_df_guns_2year <- tibble()
for (name in unique(df_detailed_guns$screen_name)){
  df_individual <- df_detailed_guns %>%
    filter(screen_name == name)
  df_individual <- df_individual[order(df_individual$created_at),]
  
  # individualize event data based on data gaps and first Tweets
  dates_individual <- dates[!format(dates, '%Y-%m') %in% data_gaps[[name]][['data_gaps']]]
  first_tweet <- as.Date(filter(df_account_info, screen_name == name)$first_tweet)
  dates_individual <- dates_individual[dates_individual >= first_tweet]
  
  for(election_date in as.list(election_dates)) {
    durations_individual = c()
    election_date <- ymd(election_date)
    dates_within_period <- dates_individual[dates_individual < election_date & dates_individual >= election_date - years(2)]
    if (length(dates_within_period) == 0) {
      next
    }
    
    for (i in 1:length(dates_within_period)) {
      temp_duration = -1
      try(temp_duration <- as.integer(filter(df_individual, created_at >= dates_within_period[i])$created_at[1] - dates_within_period[i]))
      if (temp_duration == -1 || is.na(temp_duration)) {
        temp_duration = min(c(150, as.integer(ymd('2023-01-31') - dates_within_period[i])))
      }
      durations_individual <- append(durations_individual, temp_duration)
    }
    reaction_df_guns_2year <- rbind(reaction_df_guns_2year, tibble('screen_name' = c(name),
                                                                   'election_date' = c(election_date),
                                                                   'mean_reaction_in_days' = c(mean(durations_individual))))
  }
}

reaction_df_guns_2year %>%
  group_by(election_date) %>%
  summarise(mean = mean(mean_reaction_in_days))

# load house election results
election_outcomes_df <- read_csv('Data/Election_Data/since-2010-house.csv')
reaction_df_guns_2year_formatted <- reaction_df_guns_2year
reaction_df_guns_2year_formatted$election_date <- as.integer(format(reaction_df_guns_2year_formatted$election_date, '%Y'))
colnames(reaction_df_guns_2year_formatted)[colnames(reaction_df_guns_2year_formatted) == 'election_date'] <- 'year'
colnames(reaction_df_guns_2year_formatted)[colnames(reaction_df_guns_2year_formatted) == 'screen_name'] <- 'screen_name'

complete_election_df <- merge(election_outcomes_df, reaction_df_guns_2year_formatted, by = c('year', 'screen_name'))

reg1 <- lm()



# ---- Analyze Tweet Performance Based on Reaction Time ----
# Setup of paths and data frames ----
topic <- 'gun_violence'

initial_df <- df_tweets_granular[[list_selector]]

if (topic == 'climate_change') {
  df_events <- read_csv('Data/Event_Data/climate_events.csv', skip = 1)
  event_dates <- ymd(df_events$`Begin Date`)
}

if (topic == 'gun_violence') {
  df_events <- read_csv('Data/Event_Data/mass_shootings.csv')
  df_events <- df_events %>%
    filter(killed >= 3 & injured >= 3)
  event_dates <- mdy(df_events$date)
}

if (topic == 'covid') {
  evnet_df <- read_csv('Data/Event_Data/covid.csv')
  event_dates <- ymd(df_events$`Begin Date`)
}

if (topic == 'capitol_riots') {
  event_dates <- ymd_hm('2021-01-06 10:30')
}


# Calculate time difference to the closest event in the past ----
df_regression_all_tweets <- tibble()
initial_df_copy <- initial_df

for (i in seq(length(event_dates), 1)) {
  current_df <- initial_df_copy %>%
    filter(ymd(created_at) >= ymd(event_dates[i])) %>%
    mutate(reaction_time = difftime(ymd(format(created_at, '%Y-%m-%d')), event_dates[i], units = 'days'))
  initial_df_copy <- filter(initial_df_copy, ymd(created_at) < ymd(event_dates[i]))
  df_regression_all_tweets <- rbind(df_regression_all_tweets, current_df)
} 

rm(initial_df_copy)

if (twitter_data_selector == 'all') {
  column_selector <- c('first_last', 'office_title', 'party_name', 'screen_name', 'followers')
  merge_by <- 'screen_name'
}
if (twitter_data_selector == 'combined') {
  column_selector <- c('first_last', 'office_title', 'party_name', 'followers')
  merge_by <- 'first_last'
}

df_regression_all_tweets <- merge(df_regression_all_tweets, select(df_politicians, all_of(column_selector)), by = merge_by) %>%
  mutate(year = format(created_at, '%Y')) %>%
  filter(!grepl('RT ', text, fixed = T))


reg_retweets <- lm(retweet_count ~ reaction_time + year + party_name + followers + followers*year, df_regression_all_tweets)
summary(reg_retweets)
reg_retweets_fixed_effects <- plm(retweet_count ~ reaction_time + party_name + reaction_time * party_name, 
                                  data = df_regression_all_tweets, 
                                  index = c('year', 'first_last'), 
                                  model = 'within')
summary(reg_retweets_fixed_effects)

reg_likes <- lm(favorite_count ~ reaction_time + year + party_name + followers + followers*year, df_regression_all_tweets)
summary(reg_likes)

df_regression_all_tweets %>%
  group_by(party_name) %>%
  summarise(like_average = mean(favorite_count),
            retweet_average = mean(retweet_count))


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

#' Does not seem like it is higly important how fast the politician reacts, it's
#' only that his first Tweet after an exogenous shock gets more attention than
#' following Tweets to the related topic, still there is an effect.
#' That just shows that is could be important to react to an important shock at
#' one point in time, however this thesis is also not massively supported



# ---- Pre- Against Post-Event Analysis ----
# Description ----
#' In this section I will look at the n days before and after an event to show
#' that while before the event there is no increase in Tweet activity, there is
#' a significant increase just after the event
#' Remaining Problems: events too close to each other

# Set-Up ----
initial_df <- df_tweets_granular[[list_selector]]

if (topic == 'climate_change') {
  df_events <- read_csv('Data/Event_Data/climate_events.csv', skip = 1)
  event_dates <- ymd(df_events$`Begin Date`)
}

if (topic == 'gun_violence') {
  df_events <- read_csv('Data/Event_Data/mass_shootings.csv')
  df_events <- df_events %>%
    filter(killed >= 10 & injured >= 3)
  event_dates <- mdy(df_events$date)
}

if (topic == 'covid') {
  evnet_df <- read_csv('Data/Event_Data/covid.csv')
  event_dates <- ymd(df_events$`Begin Date`)
}

if (topic == 'capitol_riots') {
  event_dates <- ymd_hm('2021-01-06 10:30')
}

df_pre_post_data <- get_tweets_pre_post_data(initial_df, event_dates, 10)
df_pre_post_data <- df_pre_post_data %>%
  mutate(year = format(created_at, '%Y'))

# Plot Day Average ----
to_plot_pre_post <- df_pre_post_data %>%
  filter(day_difference != 'outside') %>%
  group_by(day_difference) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_tweets))

to_plot_pre_post$day_difference <- as.integer(to_plot_pre_post$day_difference)

ggplot(to_plot_pre_post, aes(day_difference, mean_daily_tweets)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(to_plot_pre_post$day_difference), max(to_plot_pre_post$day_difference), 1))


# Regression Deviation From Average ----
df_pre_post_data$day_difference <- as.factor(df_pre_post_data$day_difference)
df_pre_post_data <- within(df_pre_post_data, day_difference <- relevel(day_difference, ref = 'outside'))
reg_pre_post_analysis <- lm(number_tweets ~ day_difference + year, df_pre_post_data)
summary(reg_pre_post_analysis)

# Adjust standard errors
cov_pre_post_analysis <- vcovHC(reg_pre_post_analysis, type = "HC1")
robust_se_pre_post_analysis <- sqrt(diag(cov_pre_post_analysis))

# Nicely formatted summary with robust standard errors in right column
stargazer(reg_pre_post_analysis, reg_pre_post_analysis, type = "text",
          se = list(NULL, robust_se_pre_post_analysis), flip = T)

# By Party ----
df_pre_post_data_by_party <- get_tweets_pre_post_data_by_party(initial_df, event_dates, 10)
df_pre_post_data_by_party <- df_pre_post_data_by_party %>%
  mutate(year = format(created_at, '%Y'))

# Plot Day Average ----
to_plot_pre_post_by_party <- df_pre_post_data_by_party %>%
  filter(day_difference != 'outside') %>%
  group_by(day_difference) %>%
  dplyr::summarise(mean_daily_tweets = mean(number_tweets))

to_plot_pre_post_by_party$day_difference <- as.integer(to_plot_pre_post_by_party$day_difference)

ggplot(to_plot_pre_post_by_party, aes(day_difference, mean_daily_tweets)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(to_plot_pre_post_by_party$day_difference), max(to_plot_pre_post_by_party$day_difference), 1))


# Regression Deviation From Average ----
df_pre_post_data_by_party$day_difference <- as.factor(df_pre_post_data_by_party$day_difference)
df_pre_post_data_by_party <- within(df_pre_post_data_by_party, day_difference <- relevel(day_difference, ref = 'outside'))
reg_pre_post_analysis_by_party <- lm(number_tweets ~ day_difference + year, df_pre_post_data_by_party)
summary(reg_pre_post_analysis_by_party)

# Adjust standard errors
cov_pre_post_analysis_by_party <- vcovHC(reg_pre_post_analysis_by_party, type = "HC1")
robust_se_pre_post_analysis_by_party <- sqrt(diag(cov_pre_post_analysis_by_party))

# Nicely formatted summary with robust standard errors in right column
stargazer(reg_pre_post_analysis_by_party, reg_pre_post_analysis_by_party, type = "text",
          se = list(NULL, robust_se_pre_post_analysis_by_party), flip = T)


# ---- Implied Events ----
df_all_tweets_topic <- df_tweets_granular[[list_selector]]
df_number_daily_tweets_topic <- df_all_tweets_topic %>%
  group_by(created_at) %>%
  summarise(n_tweets = n())

ggplot(df_number_daily_tweets_topic, aes(ymd(created_at), n_tweets)) +
  geom_line()

implied_events <- filter(df_number_daily_tweets_topic, n_tweets > 150)

