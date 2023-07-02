#' The purpose of this script is to summarize Twitter account behavior observed
#' in the available data to find irregularities which where caused by the data
#' collection and could heavily bias results in the analysis.
#' One example of an irregularity could be due to the collection of Tweet data
#' from different sources. Twitter seems to limit the amount of Tweets one can
#' access per user meaning that for highly active accounts it is only possible
#' to download data for the past two year making me rely on addition data as well.
#' This could lead to time gaps in data where the algorithm will wrongly assume
#' that an account was inactive. To make sure that this does not happen this
#' script will summarize the date of the first Tweet and detect any data gaps.
rm(list=ls())

library(tidyverse)
library(Microsoft365R)

source("helper_functions.R")

od_drive <- get_business_onedrive()

# ---- Set parameters ----
type = 'governor'
#' To process the 'combined' data, 'all' has to be run first
twitter_data_selector <- 'combined'

if (type == 'senate') {
  data_path <- 'Data/Account_Data/senate.csv'
  one_drive_data_path <- ifelse(twitter_data_selector == 'all', 
                                "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate/", 
                                "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Senate_Single/")
  gaps_list_output_path <- ifelse(twitter_data_selector == 'all', 
                                  'Data/Data_Gaps/senate_all_gaps.RDS',
                                  'Data/Data_Gaps/senate_all_gaps_combined.RDS')
  summary_output_path <- ifelse(twitter_data_selector == 'all', 
                                'Data/Data_Gaps/senate_gap_overview.csv',
                                'Data/Data_Gaps/senate_gap_overview_combined.csv')
  if (twitter_data_selector == 'combined') {
    list_all_gaps <- readRDS('Data/Data_Gaps/senate_all_gaps.RDS')
    df_all_overview <- read_csv('Data/Data_Gaps/senate_gap_overview.csv')
    df_politicians <- read_csv('Data/Politicians_Data/overview.csv') %>%
      filter(office_title %in% c('Senator', 'Senate Candidate'))
  }
}

if (type == 'house') {
  data_path <- ''
  one_drive_data_path <- ifelse(twitter_data_selector == 'all', 
                                "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House/",
                                "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/House_Single/")
  gaps_list_output_path <- ifelse(twitter_data_selector == 'all', 
                                  'Data/Data_Gaps/house_all_gaps.RDS',
                                  'Data/Data_Gaps/house_all_gaps_combined.RDS')
  summary_output_path <- ifelse(twitter_data_selector == 'all', 
                                'Data/Data_Gaps/house_gap_overview.csv',
                                'Data/Data_Gaps/house_gap_overview_combined.csv')
}

if (type == 'governor') {
  data_path <- ''
  one_drive_data_path <- ifelse(twitter_data_selector == 'all', 
                                "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor/",
                                "Bachelor/Bachelorarbeit/code_data/Data/Tweet_Data/Governor_Single/")
  gaps_list_output_path <- ifelse(twitter_data_selector == 'all', 
                                  'Data/Data_Gaps/governor_all_gaps.RDS',
                                  'Data/Data_Gaps/governor_all_gaps_combined.RDS')
  summary_output_path <- ifelse(twitter_data_selector == 'all', 
                                'Data/Data_Gaps/governor_gap_overview.csv',
                                'Data/Data_Gaps/governor_gap_overview_combined.csv')
}

# load account data
# account_df <- read_csv(file_path)

#' finding all data gaps, first recorded Tweet and boolean which is True if there
#' exist data gaps and false otherwise

account_info_list <- get_account_descriptions(od_drive, one_drive_data_path, twitter_data_selector)
summary_df <- account_info_list[['summary_df']]
summary_df <- summary_df[!duplicated(summary_df),]
account_info_list_copy <- account_info_list[-length(account_info_list)]

# if (twitter_data_selector == 'combined') {
#   all_politicians <- unique(df_politicians$first_last)
#   account_info_list_copy <- list()
#   
#   for (i in 1:length(all_politicians)) {
#     all_accounts <- filter(df_politicians, first_last == all_politicians[i])$screen_name
#     
#     if (length(all_accounts) == 1) {
#       summary_df <- rbind(summary_df, filter(df_all_overview, screen_name %in% all_accounts))
#       account_info_list_copy[[str_remove_all(all_politicians[i], ' ')]] <- list_all_gaps[[all_accounts[1]]]
#     }
#     
#     if (length(all_accounts) == 2) {
#       for (j in all_accounts) {
#         data_gaps <- 
#       }
#       df_summary_entry <- 
#       summary_df <- rbind(summary_df, filter(df_all_overview, screen_name %in% all_accounts))
#       account_info_list_copy[[str_remove_all(all_politicians[i], ' ')]] <- list_all_gaps[[all_accounts[1]]]
#     }
#   }
# }


#' The following section identifies the month of the first tweet of the most current
#' series of consecutive monthly Tweets. If there are no data gaps, this will be
#' the first collected Tweet, otherwise it'll be the months after the last data
#' gap recorded. This information gets stored in the summary data frame.
beginning_uninterupted <- c()

for (key in names(account_info_list_copy)){
  if (length(account_info_list_copy[[key]][['data_gaps']]) == 0) {
    start_uninterupted_data <- as.Date(account_info_list_copy[[key]][["first_tweet"]], '%Y-%m-%d')
  }
  else {
    start_uninterupted_data <- as.Date(paste(account_info_list_copy[[key]][['data_gaps']][length(account_info_list_copy[[key]][['data_gaps']])],
                                             '-01', sep = '')) + months(1)
  }
  beginning_uninterupted <- append(beginning_uninterupted, start_uninterupted_data)
}

summary_df <- cbind(summary_df, tibble(beginning_uninterupted))

# save data
saveRDS(account_info_list_copy, file = gaps_list_output_path)
write_csv(summary_df, summary_output_path)