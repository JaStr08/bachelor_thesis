#' The purpose of this script is to aggregate the Tweet data with one data set
#' per politician. To do so I decided to use the data of the most active account
#' for each month. The reasoning can be found in the paper.

rm(list=ls())

library(tidyverse)
library(Microsoft365R)

# source('helper_functions.R')

od_drive <- get_business_onedrive()

type = 'governor'

if (type == 'senate') {
  tweet_dir_data_path_od <- "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate/"
  df_data_gaps <- readRDS('Data/Data_Gaps/senate_all_gaps.RDS')
  df_account_info <- read_csv('Data/Data_Gaps/senate_gap_overview.csv')
  output_path <- 'Data/Tweet_Data/Senate_Single/'
  df_politicians <- read_csv('Data/Politicians_Data/overview.csv') %>%
    filter(office_title %in% c('Senator', 'Senate Candidate'))
}

if (type == 'house') {
  tweet_dir_data_path_od <- "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House/"
  df_data_gaps <- readRDS('Data/Data_Gaps/house_all_gaps.RDS')
  df_account_info <- read_csv('Data/Data_Gaps/house_gap_overview.csv')
  output_path <- 'Data/Tweet_Data/House_Single/'
  df_politicians <- read_csv('Data/Politicians_Data/overview.csv') %>%
    filter(office_title %in% c('House Representative', 'House Candidate'))
}

if (type == 'governor') {
  tweet_dir_data_path_od <- "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor/"
  df_data_gaps <- readRDS('Data/Data_Gaps/governor_all_gaps.RDS')
  df_account_info <- read_csv('Data/Data_Gaps/governor_gap_overview.csv')
  output_path <- 'Data/Tweet_Data/Governor_Single/'
  df_politicians <- read_csv('Data/Politicians_Data/overview.csv') %>%
    filter(office_title %in% c('Governor', 'Gubernatorial Candidate'))
}

all_politicians <- unique(df_politicians$first_last)

unprocessed <- c()

for (i in 1:length(all_politicians)) {
  all_accounts <- unique(filter(df_politicians, first_last == all_politicians[i])$screen_name)
  df_agg_account <- tibble()
  
  if (length(all_accounts) == 1) {
    od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[1], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE)
    df_only_account <- read_csv("temp_storage.csv") %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[1])
    write_csv(df_only_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
  }
  
  # logic if politician has two accounts
  if (length(all_accounts) == 2) {
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[1], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_first_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[2], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_second_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    if (class(df_first_account) == 'try-error') {
      df_second_account <- df_second_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[2])
      write_csv(df_second_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_second_account) == 'try-error') {
      df_first_account <- df_first_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[1])
      write_csv(df_first_account, paste(output_path, str_remove_all(all_politicians[i], ' '), sep = ''))
      next
    }
    df_first_account <- df_first_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[1])
    df_second_account <- df_second_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[2])
    min_first_tweet <- min(filter(df_account_info,df_account_info$screen_name %in% all_accounts)$first_tweet)
    all_year_months <-  format(seq(ymd(min_first_tweet), ymd('2023-03-31'), 'months'), '%Y-%m')
    cols_to_keep <- intersect(colnames(df_first_account), colnames(df_second_account))
    df_first_account <- df_first_account %>%
      select(cols_to_keep)
    df_second_account <- df_second_account %>%
      select(cols_to_keep)
    
    for (j in 1:length(all_year_months)) {
      df_first_account_filtered <- filter(df_first_account, year_month == all_year_months[j])
      df_second_account_filtered <- filter(df_second_account, year_month == all_year_months[j])
      
      if (nrow(df_first_account_filtered) == 0 & nrow(df_second_account_filtered) == 0) {
        next
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) > nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_first_account_filtered)
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) < nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_second_account_filtered)
      }
    }
    write_csv(df_agg_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
  }
  
  # logic if politician has three accounts
  if (length(all_accounts) == 3) {
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[1], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_first_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[2], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_second_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[3], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_third_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    if (class(df_first_account) == 'try-error' & class(df_third_account) == 'try-error') {
      df_second_account <- df_second_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[2])
      write_csv(df_second_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_second_account) == 'try-error' & class(df_third_account) == 'try-error') {
      df_first_account <- df_first_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[1])
      write_csv(df_first_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_first_account) == 'try-error' & class(df_second_account) == 'try-error') {
      df_third_account <- df_third_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[3])
      write_csv(df_third_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_first_account) == 'try-error') {
      unprocessed <- append(unprocessed, all_politicians[i])
      next
    }
    if (class(df_second_account) == 'try-error') {
      unprocessed <- append(unprocessed, all_politicians[i])
      next
    }
    if (class(df_third_account) == 'try-error') {
      unprocessed <- append(unprocessed, all_politicians[i])
      next
    }
    df_first_account <- df_first_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[1])
    df_second_account <- df_second_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[2])
    df_third_account <- df_third_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[3])

    min_first_tweet <- min(filter(df_account_info, df_account_info$screen_name %in% all_accounts)$first_tweet)
    all_year_months <-  format(seq(ymd(min_first_tweet), ymd('2023-03-31'), 'months'), '%Y-%m')
    
    cols_to_keep <- intersect(intersect(colnames(df_first_account), colnames(df_second_account)), colnames(df_third_account))
    df_first_account <- df_first_account %>%
      select(cols_to_keep)
    df_second_account <- df_second_account %>%
      select(cols_to_keep)
    df_third_account <- df_third_account %>%
      select(cols_to_keep)

    for (j in 1:length(all_year_months)) {
      df_first_account_filtered <- filter(df_first_account, year_month == all_year_months[j])
      df_second_account_filtered <- filter(df_second_account, year_month == all_year_months[j])
      df_third_account_filtered <- filter(df_third_account, year_month == all_year_months[j])

      if (nrow(df_first_account_filtered) == 0 & nrow(df_second_account_filtered) == 0 & nrow(df_third_account_filtered) == 0) {
        next
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) > nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) &
          nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) > nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_first_account_filtered)
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) < nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) &
          nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) > nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_second_account_filtered)
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) < nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T))) &
          nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) < nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_third_account_filtered)
      }
    }
    write_csv(df_agg_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
  }
  
  # logic if politician has four accounts
  if (length(all_accounts) == 4) {
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[1], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_first_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[2], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_second_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[3], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_third_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    try(od_drive$download_file(paste(tweet_dir_data_path_od, all_accounts[4], '.csv', sep=""), "temp_storage.csv", overwrite = TRUE), silent = TRUE)
    df_fourth_account <- try(read_csv("temp_storage.csv"), silent = TRUE)
    file.remove('temp_storage.csv')
    if (class(df_first_account) == 'try-error' & class(df_third_account) == 'try-error' & class(df_fourth_account) == 'try-error') {
      df_second_account <- df_second_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[2])
      write_csv(df_second_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_second_account) == 'try-error' & class(df_third_account) == 'try-error' & class(df_fourth_account) == 'try-error') {
      df_first_account <- df_first_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[1])
      write_csv(df_first_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_first_account) == 'try-error' & class(df_second_account) == 'try-error' & class(df_fourth_account) == 'try-error') {
      df_third_account <- df_third_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[3])
      write_csv(df_third_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_first_account) == 'try-error' & class(df_second_account) == 'try-error' & class(df_third_account) == 'try-error') {
      df_fourth_account <- df_fourth_account %>%
        mutate(year_month = format(created_at, '%Y-%m')) %>%
        mutate(screen_name = all_accounts[4])
      write_csv(df_fourth_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
      next
    }
    if (class(df_first_account) == 'try-error' | class(df_second_account) == 'try-error' | class(df_third_account) == 'try-error' | class(df_fourth_account) == 'try-error') {
      unprocessed <- append(unprocessed, all_politicians[i])
      next
    }
    df_first_account <- df_first_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[1])
    df_second_account <- df_second_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[2])
    df_third_account <- df_third_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[3])
    df_fourth_account <- df_fourth_account %>%
      mutate(year_month = format(created_at, '%Y-%m')) %>%
      mutate(screen_name = all_accounts[4])
    
    min_first_tweet <- min(filter(df_account_info,df_account_info$screen_name %in% all_accounts)$first_tweet)
    all_year_months <-  format(seq(ymd(min_first_tweet), ymd('2023-03-31'), 'months'), '%Y-%m')
    
    cols_to_keep <- intersect(intersect(intersect(colnames(df_first_account), colnames(df_second_account)), colnames(df_third_account)), colnames(df_third_account))
    df_first_account <- df_first_account %>%
      select(cols_to_keep)
    df_second_account <- df_second_account %>%
      select(cols_to_keep)
    df_third_account <- df_third_account %>%
      select(cols_to_keep)
    df_fourth_account <- df_fourth_account %>%
      select(cols_to_keep)
    
    for (j in 1:length(all_year_months)) {
      df_first_account_filtered <- filter(df_first_account, year_month == all_year_months[j])
      df_second_account_filtered <- filter(df_second_account, year_month == all_year_months[j])
      df_third_account_filtered <- filter(df_third_account, year_month == all_year_months[j])
      
      if (nrow(df_first_account_filtered) == 0 & nrow(df_second_account_filtered) == 0 & nrow(df_third_account_filtered) == 0) {
        next
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) > nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) &
          nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) > nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_first_account_filtered)
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) < nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) &
          nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) > nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_second_account_filtered)
      }
      if (nrow(filter(df_first_account_filtered, !grepl('RT ', text, fixed = T))) < nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T))) &
          nrow(filter(df_second_account_filtered, !grepl('RT ', text, fixed = T))) < nrow(filter(df_third_account_filtered, !grepl('RT ', text, fixed = T)))) {
        df_agg_account <- rbind(df_agg_account, df_third_account_filtered)
      }
    }
    write_csv(df_agg_account, paste(output_path, str_remove_all(all_politicians[i], ' '), '.csv', sep = ''))
  }
}
