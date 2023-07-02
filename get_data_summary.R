#' This script is used to create a general overview of the data including the
#' plots which can be found in the data section of the paper.

rm(list=ls())

library(tidyverse)
library(Microsoft365R)
library(lubridate)

source("helper_functions.R")

od_drive <- get_business_onedrive()

# Load Politician Data ----
# load simple politician data
df_politicians <- read_csv('Data/Politicians_Data/overview.csv')
df_politicians$party_name <- factor(df_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_politicians)[colnames(df_politicians) == 'followers_count'] <- 'followers'

# load politician data by election
df_election_politicians <- read_csv('Data/Politicians_Data/elections_without_governors.csv')
df_election_politicians <- merge(select(df_politicians, -c(state)), select(df_election_politicians, c(year, state_po, first_last, candidatevotes, totalvotes, share, elected, incumbent)), by = 'first_last')
df_election_politicians$party_name <- factor(df_election_politicians$party_name, levels = c('Republican', 'Democrat', 'Libertarian'))
colnames(df_election_politicians)[colnames(df_election_politicians) == 'state_po'] <- 'state'
colnames(df_election_politicians)[colnames(df_election_politicians) == 'followers_count'] <- 'followers'
df_election_politicians <- df_election_politicians %>%
  mutate(previous_election_year = year - ifelse(grepl('Senat', office_title), 2, 2)) %>%
  rowwise() %>%
  mutate(election_day = get_election_day(year)) %>%
  mutate(past_election_day = get_election_day(previous_election_year))

all_types <- c('senate', 'house', 'governor')
all_twitter_data_types <- c('combined', 'all')

df_total_accounts <- tibble()
df_total_tweets <- tibble()
df_total_tweets_per_month <- tibble()
df_total_accounts_per_month <- tibble()
df_election_accounts <- tibble()
df_election_tweets <- tibble()
df_election_accounts_per_month <- tibble()
df_election_tweets_per_month <- tibble()

for (i in 1:length(all_types)) {
  for (j in 1:length(all_twitter_data_types)) {
    # ---- Set Up ----
    # Parameter Selection ----
    type <- all_types[i]
    #' options:
    #' - all: every single Twitter account
    #' - combined: combined Twitter accounts for each politician (for each months the
    #'             Tweets of the more active account are taken)
    twitter_data_selector <- all_twitter_data_types[j]
    
    # potential additional topics: immigration
    topics <- c('climate_change', 'january-6', 'gun_laws', 'covid')
    
    
    # Get Data Paths and Set Selectors ----
    if (type == 'senate') {
      tweet_dir_data_path_od <- ifelse(twitter_data_selector == 'all', 
                                       "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate/", 
                                       "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate_Single/")
    }
    
    if (type == 'house') {
      tweet_dir_data_path_od <- ifelse(twitter_data_selector == 'all', 
                                       "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House/",
                                       "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House_Single/")
    }
    
    if (type == 'governor') {
      tweet_dir_data_path_od <- ifelse(twitter_data_selector == 'all', 
                                       "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor/",
                                       "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor_Single/")
    }
    
    
    # Load Twitter Data ----
    # get Twitter data for selected topics + account summary data frame
    df_tweets_granular <- read_all_files_filtered(od_drive, tweet_dir_data_path_od, topics, twitter_data_selector)
    
    
    # ---- Data Summary ----
    # Tweets ----
    df_total_tweets_per_month_comb <- df_tweets_granular[['activity']] %>%
      group_by(year_month) %>%
      summarise(total_tweets = sum(n)) %>%
      mutate(date = ymd(paste(year_month, '-01', sep=''))) %>%
      mutate(twitter_data = twitter_data_selector) %>%
      mutate(office_type = type)
    
    df_total_tweets_per_month <- rbind(df_total_tweets_per_month, df_total_tweets_per_month_comb)
    
    # Accounts ----
    df_total_accounts_per_month_comb <- df_tweets_granular[['activity']] %>%
      group_by(year_month) %>%
      summarise(total_accounts = length(unique(file_name))) %>%
      mutate(date = ymd(paste(year_month, '-01', sep=''))) %>%
      mutate(twitter_data = twitter_data_selector) %>%
      mutate(office_type = type)
    
    df_total_accounts_per_month <- rbind(df_total_accounts_per_month, df_total_accounts_per_month_comb)
    
    total_accounts <- length(unique(df_tweets_granular[['activity']]$file_name))
    df_total_accounts <- rbind(df_total_accounts, tibble(office_type = type, 
                                                         twitter_data_type = twitter_data_selector,
                                                         number_accounts = total_accounts))
    
    df_total_tweets <- rbind(df_total_tweets, tibble(office_type = type, 
                                                     twitter_data_type = twitter_data_selector,
                                                     number_tweets = sum(df_tweets_granular[['activity']]$n)))
    
    # Elections ----
    if (type != 'governor') {
      #' get the number of accounts or participating politicians for each election
      #' same for the number of Tweets posted by the participating accounts/politicians
      #' during the two years before election
      if (type == 'senate') {
        filter_keyword <- 'Senate'
      }
      if (type == 'house') {
        filter_keyword <- 'House'
      }
      df_tweet_overview <- df_tweets_granular[['activity']] %>%
        mutate(year_month_day = ymd(paste(year_month, '-01', sep = '')))
      df_election_filtered <- filter(df_election_politicians, grepl(filter_keyword, office_title))
      election_days <- sort(ymd(as.Date(setdiff(unique(df_election_filtered$election_day), ymd('2021-11-02')), origin = "1970-01-01")))
      previous_election_days <- sort(ymd(as.Date(setdiff(unique(df_election_filtered$past_election_day), ymd('2021-11-02')), origin = "1970-01-01")))
      for (k in 1:length(election_days)) {
        if (twitter_data_selector == 'combined') {
          election_participants <- unique(filter(df_election_filtered, election_day == election_days[k])$first_last)
          df_tweets_filtered <- df_tweet_overview %>%
            filter(first_last %in% election_participants) %>%
            filter(year_month_day <= election_days[k] & year_month_day > previous_election_days[k])
        }
        if (twitter_data_selector == 'all') {
          election_participants <- unique(filter(df_election_filtered, election_day == election_days[k])$screen_name)
          df_tweets_filtered <- df_tweet_overview %>%
            filter(screen_name %in% election_participants) %>%
            filter(year_month_day <= election_days[k] & year_month_day > previous_election_days[k])
        }
        
        df_election_accounts <- rbind(df_election_accounts, tibble(office_type = type, 
                                                                   twitter_data_type = twitter_data_selector, 
                                                                   election_year = format(election_days[k], "%Y"),
                                                                   number_accounts = length(unique(df_tweets_filtered$file_name))))
        df_election_tweets <- rbind(df_election_tweets, tibble(office_type = type, 
                                                               twitter_data_type = twitter_data_selector, 
                                                               election_year = format(election_days[k], "%Y"),
                                                               number_tweets = sum(df_tweets_filtered$n)))
        
        df_election_total_tweets_per_month_comb <- df_tweets_filtered %>%
          select(-c(year_month_day)) %>%
          group_by(year_month) %>%
          summarise(total_tweets = sum(n)) %>%
          mutate(date = ymd(paste(year_month, '-01', sep=''))) %>%
          mutate(twitter_data = twitter_data_selector) %>%
          mutate(office_type = type)
        
        df_election_tweets_per_month <- rbind(df_election_tweets_per_month, df_election_total_tweets_per_month_comb)
        
        # Accounts ----
        df_election_total_accounts_per_month_comb <- df_tweets_filtered %>%
          select(-c(year_month_day)) %>%
          group_by(year_month) %>%
          summarise(total_accounts = length(unique(file_name))) %>%
          mutate(date = ymd(paste(year_month, '-01', sep=''))) %>%
          mutate(twitter_data = twitter_data_selector) %>%
          mutate(office_type = type)
        
        df_election_accounts_per_month <- rbind(df_election_accounts_per_month, df_election_total_accounts_per_month_comb)
      }
    }
  }
}

# write_csv(df_total_accounts, 'Data/Data_Summary/number_accounts.csv')
# write_csv(df_total_tweets_per_month, 'Data/Data_Summary/number_tweets_by_month.csv')
# write_csv(df_total_accounts_per_month, 'Data/Data_Summary/number_accounts_by_month.csv')
# write_csv(df_election_accounts, 'Data/Data_Summary/number_accounts_by_election.csv')
# write_csv(df_election_tweets, 'Data/Data_Summary/number_tweets_by_election.csv')
# write_csv(df_election_accounts_per_month, 'Data/Data_Summary/number_accounts_by_election_month.csv')
# write_csv(df_election_tweets_per_month, 'Data/Data_Summary/number_tweets_by_election_month.csv')

# ---- Plot Data Summary ----
# Load Data ----
df_total_accounts <- read_csv('Data/Data_Summary/number_accounts.csv')
df_total_tweets_per_month <- read_csv('Data/Data_Summary/number_tweets_by_month.csv')
df_total_accounts_per_month <- read_csv('Data/Data_Summary/number_accounts_by_month.csv')
df_election_accounts <- read_csv('Data/Data_Summary/number_accounts_by_election')
df_election_tweets <- read_csv('Data/Data_Summary/number_tweets_by_election.csv')
df_election_accounts_per_month <- read_csv('Data/Data_Summary/number_accounts_by_election_month.csv')
df_election_tweets_per_month <- read_csv('Data/Data_Summary/number_tweets_by_election_month.csv')

# Plot Monthly Tweets By Number of Accounts ----
colnames(df_total_tweets_per_month)[colnames(df_total_tweets_per_month) == 'total_tweets'] <- 'count'
df_total_tweets_per_month$office_type <- factor(df_total_tweets_per_month$office_type,
                                                levels = c('senate', 'house', 'governor'),
                                                labels = c('Senate', 'House', 'Governor'))
df_total_tweets_per_month$twitter_data <- factor(df_total_tweets_per_month$twitter_data,
                                                levels = c('all', 'combined'),
                                                labels = c('All Accounts', 'Combined Accounts'))

colnames(df_total_accounts_per_month)[colnames(df_total_accounts_per_month) == 'total_accounts'] <- 'count'
df_total_accounts_per_month$office_type <- factor(df_total_accounts_per_month$office_type,
                                                levels = c('senate', 'house', 'governor'),
                                                labels = c('Senate', 'House', 'Governor'))
df_total_accounts_per_month$twitter_data <- factor(df_total_accounts_per_month$twitter_data,
                                                 levels = c('all', 'combined'),
                                                 labels = c('All Accounts', 'Combined Accounts'))

ggplot() +
  geom_bar(data = df_total_accounts_per_month, aes(date, 100*count), stat = 'identity', color = 'grey') +
  geom_line(data = df_total_tweets_per_month, aes(date, count), color = 'black') +
  scale_y_continuous(name = 'Number of Tweets',
                     sec.axis = sec_axis(~./100, name = 'Number of Accounts')) +
  facet_grid(office_type ~ twitter_data, scales = 'free_y') +
  theme_light() +
  theme(
    axis.title.y = element_text(color = 'black'),
    axis.title.y.right = element_text(color = 'black'),
    axis.text.y = element_text(color = 'black'),
    axis.text.y.right = element_text(color = 'grey'),
    strip.background = element_rect(fill = 'grey19')
  )


# Plot Monthly Election Tweets By Number of Accounts ----
colnames(df_election_tweets_per_month)[colnames(df_election_tweets_per_month) == 'total_tweets'] <- 'count'
df_election_tweets_per_month$office_type <- factor(df_election_tweets_per_month$office_type,
                                                levels = c('senate', 'house', 'governor'),
                                                labels = c('Senate', 'House', 'Governor'))
df_election_tweets_per_month$twitter_data <- factor(df_election_tweets_per_month$twitter_data,
                                                 levels = c('all', 'combined'),
                                                 labels = c('All Accounts', 'Combined Accounts'))

colnames(df_election_accounts_per_month)[colnames(df_election_accounts_per_month) == 'total_accounts'] <- 'count'
df_election_accounts_per_month$office_type <- factor(df_election_accounts_per_month$office_type,
                                                  levels = c('senate', 'house', 'governor'),
                                                  labels = c('Senate', 'House', 'Governor'))
df_election_accounts_per_month$twitter_data <- factor(df_election_accounts_per_month$twitter_data,
                                                   levels = c('all', 'combined'),
                                                   labels = c('All Accounts', 'Combined Accounts'))

ggplot() +
  geom_bar(data = df_election_accounts_per_month, aes(date, 100*count), stat = 'identity', color = 'grey') +
  geom_line(data = df_election_tweets_per_month, aes(date, count), color = 'black') +
  scale_y_continuous(name = 'Number of Tweets',
                     sec.axis = sec_axis(~./100, name = 'Number of Accounts')) +
  facet_grid(office_type ~ twitter_data, scales = 'free_y') +
  theme_light() +
  theme(
    axis.title.y = element_text(color = 'black'),
    axis.title.y.right = element_text(color = 'grey'),
    axis.text.y = element_text(color = 'black'),
    axis.text.y.right = element_text(color = 'black'),
    strip.background = element_rect(fill = 'grey19')
  )
