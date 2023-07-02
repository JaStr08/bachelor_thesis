#' This scripts contains helper function used across the main data preparation
#' and analysis scripts. So, this is kind of my own small libratry created for
#' this project. The docstring can be found within each function explaining
#' the purpose of each specific one.

library(tidyverse)
library(Microsoft365R)
library(testit)

# ---- Data Cleaning ----

# create first_name, last_name and first_last column in data frame
split_name <- function(df, name_vector) {
  # remove current first and last name column if it exist
  if ('first_name' %in% colnames(df)) {
    df <- select(df, -c('first_name'))
  }
  if ('last_name' %in% colnames(df)) {
    df <- select(df, -c('last_name'))
  }
  
  df_names_split <- as.data.frame(str_split_fixed(str_to_title(name_vector), " ", -1))
  
  df_first_names_extracted <- df_names_split
  for (i in (2:ncol(df_first_names_extracted))) {
    for (j in 1:nrow(df_first_names_extracted)) {
      if (str_length(str_remove(df_first_names_extracted[[j, 1]], '\\.')) <= 1) {
        df_first_names_extracted[j, 1] <- df_first_names_extracted[j, i]
      }
    }
  }
  df_first_names_extracted$V1 <- gsub('"', '', df_first_names_extracted$V1)
  
  df_last_names_extracted <- df_names_split
  for (i in rev(2:ncol(df_last_names_extracted))) {
    for (j in 1:nrow(df_last_names_extracted)) {
      if (!df_last_names_extracted[j, i] %in% c("Jr.", "", "Iii", "Sr.", "Jr", "Sr")) {
        df_last_names_extracted[j, i-1] <- df_last_names_extracted[j, i]
      }
    }
  }
  df <- as.data.frame(cbind(df, 
                            df_first_names_extracted[, 1], 
                            df_last_names_extracted[, 1]))
  colnames(df)[colnames(df) %in% c('df_first_names_extracted[, 1]', 'df_last_names_extracted[, 1]')] <- c('first_name', 'last_name')
  df <- df %>%
    mutate(first_last = paste(first_name, last_name, sep = ' '))
  
  # remove temporary objects
  # rm(df_names_split, df_first_names_extracted, df_last_names_extracted)
  
  return(df)
}

# ----

get_election_day <- function(year) {
  #' returns the date of the election year for a specific year which is always
  #' the Tuesday after the first Monday in November
  year = as.character(year)
  
  if (length(year) == 1) {
    potential_days = seq(ymd(paste(year, '-11-02', sep='')), ymd(paste(year, '-11-08', sep = '')), 'days')
    return(potential_days[wday(potential_days, label = TRUE, abbr = FALSE) == 'Tuesday'])
  }
  if (length(year) > 1) {
    election_days <- ymd(as.Date(sapply(year, function(x) sub_get_election_day(x)), origin = "1970-01-01"))
    return(election_days)
  }
}

sub_get_election_day <- function(year) {
  potential_days = seq(ymd(paste(year, '-11-02', sep='')), ymd(paste(year, '-11-08', sep = '')), 'days')
  return(potential_days[wday(potential_days, label = TRUE, abbr = FALSE) == 'Tuesday'])
}

get_distance_to_next_election <- function(timestamp) {
  #' returns the distance in days to the next upcoming election day
  all_election_years <- c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024, 2026)
  all_election_days <- c()
  for (i in 1:length(all_election_years)) {
    all_election_days <- append(all_election_days, get_election_day(all_election_years[i]))
  }
  if (length(timestamp) == 1) {
    next_election_day <- map_next_event_day(timestamp, all_election_days)
    return(difftime(ymd(next_election_day), ymd(timestamp), units = 'days'))
  }
  if (length(timestamp) > 1) {
    next_elextion_days <- ymd(as.Date(sapply(timestamp, function(x) map_next_event_day(x, all_election_days)), origin = "1970-01-01"))
    return(difftime(ymd(next_elextion_days), ymd(timestamp), units = 'days'))
  }
}

get_distance_to_next_event <- function(timestamp, event_dates) {
  #' returns the distance in days to the next upcoming election day
  if (length(timestamp) == 1) {
    next_event_day <- map_next_event_day(timestamp, event_dates)
    return(difftime(ymd(next_election_day), ymd(timestamp), units = 'days'))
  }
  if (length(timestamp) > 1) {
    next_event_days <- ymd(as.Date(sapply(timestamp, function(x) map_next_event_day(x, event_dates)), origin = "1970-01-01"))
    return(difftime(ymd(next_event_days), ymd(timestamp), units = 'days'))
  }
}

get_distance_from_past_event <- function(timestamp, event_dates) {
  #' returns the distance in days to the next upcoming election day
  if (length(timestamp) == 1) {
    previous_event_day <- map_previous_event_day(timestamp, event_dates)
    return(difftime(ymd(timestamp), ymd(previous_event_day), units = 'days'))
  }
  if (length(timestamp) > 1) {
    previous_event_day <- ymd(as.Date(unlist(sapply(timestamp, function(x) map_previous_event_day(x, event_dates))), origin = "1970-01-01"))
    return(difftime(ymd(timestamp), ymd(previous_event_day), units = 'days'))
  }
}

get_next_election_day <- function(timestamp) {
  #' returns the distance in days to the next upcoming election day
  all_election_years <- c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024, 2026)
  all_election_days <- c()
  for (i in 1:length(all_election_years)) {
    all_election_days <- append(all_election_days, get_election_day(all_election_years[i]))
  }
  if (length(timestamp) == 1) {
    return(map_next_event_day(timestamp, all_election_days))
  }
  if (length(timestamp) > 1) {
    return(ymd(as.Date(sapply(timestamp, function(x) map_next_event_day(x, all_election_days)), origin = "1970-01-01")))
  }
}

map_next_event_day <- function(timestamp, all_event_days) {
  #' returns the next upcoming election day for a timestamp
  next_event_day <- sort(all_event_days[all_event_days >= timestamp])[1]
  return(next_event_day)
}

map_previous_event_day <- function(timestamp, all_event_days) {
  #' returns the next upcoming election day for a timestamp
  all_past_events <- sort(all_event_days[all_event_days <= timestamp])
  if (length(all_past_events) > 0) {
    past_event_day <- all_past_events[as.integer(length(all_past_events))]
  }
  if (length(all_past_events) == 0) {
    past_event_day <- NA
  }
  return(past_event_day)
}


read_all_files_filtered <- function(od, directory_path, filters, selector){
  #' reads the data by topic to save computer storage by avoid loading all the
  #' data, that's also why I saved the Tweets of each politician independently
  #' allowing me do to this storage optimization
  total_data <- list()
  count_list <- 1
  for (item in filters){
    total_data[[count_list]] = tibble()
    names(total_data)[count_list] <- item
    count_list <- count_list + 1
  }
  total_data[[count_list]] <- tibble()
  names(total_data)[count_list] <- "activity"
  files <- od$list_files(path=directory_path)
  files <- files$name
  for (file in files){
    print(file)
    od$download_file(paste(directory_path, file, sep=""), "temp_storage.csv", overwrite = TRUE)
    temp <- read_csv("temp_storage.csv")
    temp$created_at <- as.Date(temp$created_at)
    total_data[["activity"]] <- bind_rows(total_data[["activity"]], get_account_info(temp, file, selector))
    if (selector == 'all') {
      temp <- temp %>%
        mutate(screen_name = substr(file, 1, str_length(file)-4))
    }
    if (selector == 'combined') {
      temp <- temp %>%
        mutate(first_last = gsub("([a-z])([A-Z])","\\1 \\2", substr(file, 1, str_length(file)-4)))
    }
    temp <- filter_data(temp, filters)
    for (item in filters){
      temp_filtered <- temp[[item]]
      if (nrow(temp_filtered) > 0){
        total_data[[item]] <- bind_rows(total_data[[item]], temp_filtered)
      }
    }
  }
  return(total_data)
}

read_all_files_filtered_to_df <- function(od, directory_paths, filters, selector){
  #' reads the data by topic to save computer storage by avoid loading all the
  #' data, that's also why I saved the Tweets of each politician independently
  #' allowing me do to this storage optimization
  #' the difference to the function above is that it returns all data in one
  #' large data frame, this is the fucntion which I ended up using for my final
  #' analysis, the one above was more for the experimentation that I could load
  #' partly data as well
  df_filtered_data <- tibble()
  cols_to_keep <- c('created_at', 'id', 'in_reply_to_status_id',
                    'in_reply_to_user_id', 'in_reply_to_screen_name', 'retweet_count',
                    'favorite_count', 'retweeted', 'text', 'topic')
  if (selector == 'combined') {
    cols_to_keep <- append(cols_to_keep, 'first_last')
  }
  if (selector == 'all') {
    cols_to_keep <- append(cols_to_keep, 'screen_name')
  }
  # df_activity <- tibble()
  for (path in directory_paths) {
    if (grepl('Senate', path)){
      election_type = 'Senate'
    }
    if (grepl('House', path)){
      election_type = 'House'
    }
    if (grepl('Governor', path)){
      election_type = 'Governor'
    }
    files <- od$list_files(path=path)
    files <- files$name
    for (file in files){
      print(file)
      od$download_file(paste(path, file, sep=""), "temp_storage.csv", overwrite = TRUE)
      temp <- read_csv("temp_storage.csv")
      temp$created_at <- ymd(as.Date(temp$created_at))
      # df_activity <- bind_rows(df_activity, mutate(get_account_info(temp, file, selector), election_type = election_type))
      if (selector == 'all') {
        temp <- temp %>%
          mutate(screen_name = substr(file, 1, str_length(file)-4))
      }
      if (selector == 'combined') {
        temp <- temp %>%
          mutate(first_last = gsub("([a-z])([A-Z])","\\1 \\2", substr(file, 1, str_length(file)-4)))
      }
      temp_filtered_data <- filter_data_df(temp, filters)
      if (nrow(temp_filtered_data) == 0) {
        next
      }
      df_filtered_data <- rbind(df_filtered_data, mutate(select(temp_filtered_data, all_of(cols_to_keep)), election_type = election_type))
    }
  }
  return(df_filtered_data)
}


filter_data <- function(df, filters){
  # function for filtering the Tweet data by topic
  filtered_data_cc <- tibble()
  filtered_data_j6 <- tibble()
  filtered_data_gun <- tibble()
  filtered_data_cov <- tibble()
  filtered_data_immigration <- tibble()
  filtered_data <- list()
  if ('climate_change' %in% filters){
    filtered_data_cc <- filter(df, (grepl('climate change|carbon footprint|pollution|sea level|ozone|carbon dioxide|global warming|natural disaster|biodiversity|C02|emission|deforestation|renewable|renewable electricity|environment.electricity|electricity.environment|environment.energy|energy.environment|biofuel.climate|climate.biofuel|green.new.deal|green.energy|energy.green|clean energy|solar power|sustainability|climate.issue|decarbonising|decarbonizing|climate system|greenhouse|coral|reef|ocean*warming|carbon*tax|climate*emergency|climate|paris agreement', tolower(df[["text"]]))))
    # filtered_data <- filter(df, (grepl("\\<irma\\>|hurricane irma|storm|hurricane|wildfire|flooding", tolower(df[["text"]])) & grepl("climate change|climate|environment|global warming|climate agreement|\\<epa\\>|paris agreement|pollution|ozone", tolower(df[["text"]]))))
    filtered_data[['climate_change']] <- filtered_data_cc
  }
  if ('january-6' %in% filters){
    filtered_data_j6 <- filter(df, (grepl("january 6|january 6th|capitol attack|attack on the capitol|capitol riots|capitol mob|paris agreement|pollution|ozone", tolower(df[["text"]]))))
    filtered_data[['january-6']] <- filtered_data_j6
  }
  if ('gun_laws' %in% filters){
    filtered_data_gun <- filter(df, (grepl("gun.violence|guns|\\<gun.\\>|gun.control|gun regulation|gun owner|gun movement|gun.safety|safe.gun|gun*law|semi.automatic|mental health checks|firearm|\\<nra\\>|national rifle assoiation|gun control|gun purchase|background checks|assault weapon|\\<ar15\\>|\\<ar-15\\>|rifle|gun policy|disarm|armed.attack|school officer|armed.guard", tolower(df[["text"]]))))
    filtered_data_gun <- filter(filtered_data_gun, (!grepl('young-?guns|young ?guns', tolower(filtered_data_gun[["text"]]))))
    filtered_data[['gun_laws']] <- filtered_data_gun
    # amok|uvalde|parkland|highland park|monterey park|
  }
  if ('immigration' %in% filters){
    filtered_data_immigration <- filter(df, (grepl("immigration|immigrant|dreamer|\\<wall\\>.Mexico|Mexico.\\<wall\\>|border.patrol|border.crisis|crisis.border|securing.border|border.secure|secure.border|refugee|daca|deport|undocumented|border crossing|remain in mexico|military.border|border.military|border.illegal|illegal.border|asylum|cross.border|\\<ice\\>.agents|\\<ice\\>.border|border.\\<ice\\>|\\<cbp\\>|open.border|migrant|border.invasion|border.troop|troop.border|illegal.mexico|\\<dps\\>|rush.border|title.42|Mexican.illegal|illegal.Mexican", tolower(df[["text"]]))))
    filtered_data[['immigration']] <- filtered_data_immigration
  }
  if ('covid' %in% filters){
    filtered_data_cov <- filter(df, (grepl("covid|vaccine|covid 19|covid-19|corona|virus|pandemic|\\<cdc\\>|quarantine|mask|masks", tolower(df[["text"]]))))
    filtered_data[['covid']] <- filtered_data_cov
  }
  return(filtered_data)
}

filter_data_df <- function(df, filters){
  #' function for filtering the Tweet data by topic, different to the one before
  #' concerning the data type it stores the data in (data frame)
  filtered_data_cc <- tibble()
  filtered_data_gun <- tibble()
  filtered_data_immigration <- tibble()
  if ('climate_change' %in% filters){
    filtered_data_cc <- df %>%
      filter((grepl('climate change|carbon footprint|pollution|sea level|ozone|carbon dioxide|global warming|natural disaster|biodiversity|C02|emission|deforestation|renewable|renewable electricity|environment.electricity|electricity.environment|environment.energy|energy.environment|biofuel.climate|climate.biofuel|green.new.deal|green.energy|energy.green|clean energy|solar power|sustainability|climate.issue|decarbonising|decarbonizing|climate system|greenhouse|coral|reef|ocean*warming|carbon*tax|climate*emergency|climate|paris agreement', tolower(df[["text"]])))) %>%
      mutate('topic' = 'climate_change')
    # filtered_data <- filter(df, (grepl("\\<irma\\>|hurricane irma|storm|hurricane|wildfire|flooding", tolower(df[["text"]])) & grepl("climate change|climate|environment|global warming|climate agreement|\\<epa\\>|paris agreement|pollution|ozone", tolower(df[["text"]]))))
    # (grepl("climate change|climate|environment|global warming|climate agreement|\\<epa\\>|paris agreement|pollution|ozone", tolower(df[["text"]])) &
    #    grepl("climate change|protect|global warming|climate agreement|\\<epa\\>|paris agreement|pollution|ozone|protection", tolower(df[["text"]])))
  }
  if ('gun_laws' %in% filters){
    filtered_data_gun <- df %>%
      filter(grepl("gun.violence|guns|\\<gun.\\>|gun.control|gun regulation|gun owner|gun movement|gun.safety|safe.gun|gun*law|semi.automatic|mental health checks|firearm|\\<nra\\>|national rifle assoiation|gun control|gun purchase|background checks|assault weapon|\\<ar15\\>|\\<ar-15\\>|rifle|gun policy|disarm|armed.attack|school officer|armed.guard|be gone act", tolower(df[["text"]]))) %>%
      mutate('topic' = 'gun_policy')
    filtered_data_gun <- filtered_data_gun %>%
      filter(!grepl('young-?guns|young ?guns', tolower(filtered_data_gun[["text"]])))
    # (grepl("gun violence|guns|shooting|armed attack|amok|uvalde|parkland|highland park|monterey park", tolower(df[["text"]])))
  }
  if ('immigration' %in% filters){
    filtered_data_immigration <- df %>%
      filter((grepl("immigration|immigrant|dreamer|\\<wall\\>.mexico|mexico.\\<wall\\>|border.patrol|border.crisis|crisis.border|securing.border|border.secure|secure.border|refugee|daca|deport|undocumented|border crossing|remain in mexico|military.border|border.military|border.illegal|illegal.border|asylum|cross.border|\\<ice\\>.agents|\\<ice\\>.border|border.\\<ice\\>|\\<cbp\\>|open.border|migrant|border.invasion|border.troop|troop.border|illegal.mexico|\\<dps\\>|rush.border|title.42|mexican.illegal|illegal.mexican|travel ban|moslems|islam|deport|illegal.alien|immigration.policy|immigration.policies|guest.worker|visa|zero tolerance|illegal.cross|security funding|green card", tolower(df[["text"]])))) %>%
      mutate('topic' = 'immigration')
  }
  filtered_data <- rbind(filtered_data_cc, filtered_data_gun, filtered_data_immigration)
  return(filtered_data)
}


get_account_info <- function(df, file, selector){
  #' function to get the tweets per month per account
  tweets_per_month_account <- df %>%
    mutate(year_month = format(created_at, "%Y-%m")) %>%
    group_by(year_month) %>%
    dplyr::summarise(n = n()) %>%
    mutate(file_name = file)
  if (selector == 'all') {
    tweets_per_month_account <- tweets_per_month_account %>%
      mutate(screen_name = substr(file, 1, str_length(file)-4))
  }
  if (selector == 'combined') {
    tweets_per_month_account <- tweets_per_month_account %>%
      mutate(first_last = gsub("([a-z])([A-Z])","\\1 \\2", substr(file, 1, str_length(file)-4)))
  }
  return(tweets_per_month_account)
}


aggregate_tweet_count <- function(df){
  #' Counts the number of Tweets per month and Twitter Account
  agg_count <- df %>%
    mutate(year_month = format(created_at, "%Y-%m")) %>%
    group_by(screen_name, year_month) %>%
    dplyr::summarise(n_topic = n())
  return(agg_count)
}

get_aggregate_df <- function(df_topic, df_activity, selector){
  #' Counts the number of Tweets per month and Twitter Account
  if (selector == 'all') {
    agg_count <- df_topic %>%
      mutate(year_month = format(created_at, "%Y-%m")) %>%
      group_by(screen_name, year_month) %>%
      dplyr::summarise(n_topic = n())
    # Merge with activity df to calculate the shares
    merged_df <- merge(agg_count,df_activity,by=c("year_month","screen_name"), all.y=TRUE)
  }
  if (selector == 'combined') {
    agg_count <- df_topic %>%
      mutate(year_month = format(created_at, "%Y-%m")) %>%
      group_by(first_last, year_month) %>%
      dplyr::summarise(n_topic = n())
    # Merge with activity df to calculate the shares
    merged_df <- merge(agg_count,df_activity,by=c("year_month","first_last"), all.y=TRUE)
  }
  merged_df$n_topic <- replace(merged_df$n_topic,is.na(merged_df$n_topic),0)
  merged_df_complete <- merged_df %>%
    mutate(share = n_topic/n)
  return(merged_df_complete)
}


plot_distribution <- function(df, column_name){
  ggplot(df, aes(x = eval(column_name))) +
    geom_histogram(binwidth = 0.01)
}

get_sd_topic <- function(df, selector){
  #' returns df including the standard deviation over all months by user
  #' measurement = 'share': standard deviation of share of topic related Tweets
  #'                        to all Tweets
  #' measurement = 'tweets': standard deviation of topic related Tweets
  if (selector == 'all') {
    sd_df <- df %>%
      group_by(screen_name) %>%
      dplyr::summarise(sd_share = sd(share),
                sd_tweets = sd(n_topic))
  }
  if (selector == 'combined') {
    sd_df <- df %>%
      group_by(first_last) %>%
      dplyr::summarise(sd_share = sd(share),
                sd_tweets = sd(n_topic))
  }
  return(sd_df)
}


to_date <- function(x){
  date_time <- paste(paste(unlist(str_split(x, ' '))[c(6, 2, 3)], collapse = '-'), unlist(str_split(x, ' '))[4], sep = ' ')
  return(date_time)
}


get_account_descriptions <- function(od, directory_path, selector){
  # function to get general data about accounts
  files <- od$list_files(path=directory_path)
  files <- files$name
  summary_list <- list()
  first_tweet <- c()
  data_gaps <- c()
  if (selector == 'all') {
    screen_name <- c()
  }
  if (selector == 'combined') {
    first_last <- c()
  }
  for (file in files){
    account_list <- list()
    od$download_file(paste(directory_path, file, sep=""), "temp_storage.csv", overwrite = TRUE)
    temp <- read_csv("temp_storage.csv")
    if (nrow(temp) == 0) {
      next
    } 
    temp$created_at <- as.Date(temp$created_at)
    temp <- temp[order(temp$created_at),]
    first_tweet <- append(first_tweet, temp$created_at[1])
    account_list[['first_tweet']] <- temp$created_at[1]
    #' get moth first and last month year, create vector with all get all month
    #' year in the data, take set difference to find moths without tweets
    month_year_range <- format(seq(temp$created_at[1], temp$created_at[length(temp$created_at)], by = "month"), '%Y-%m')
    month_year_data <- unique(format(temp$created_at, '%Y-%m'))
    month_year_gaps <- setdiff(month_year_range, month_year_data)
    account_list[['data_gaps']] <- month_year_gaps
    gap_bool <- ifelse(length(month_year_gaps) == 0, F, T)
    data_gaps <- append(data_gaps, gap_bool)
    if (selector == 'all') {
      screen_name_ind <- unlist(str_split(file, '\\.'))[1]
      screen_name <- append(screen_name, screen_name_ind)
      summary_list[[screen_name_ind]] <- account_list
    }
    if (selector == 'combined') {
      first_last_ind <- gsub("([a-z])([A-Z])","\\1 \\2", unlist(str_split(file, '\\.'))[1])
      first_last <- append(first_last, first_last_ind)
      summary_list[[first_last_ind]] <- account_list
    }
  }
  if (selector == 'all') {
    summary_df <- tibble(first_tweet, data_gaps, screen_name)
  }
  if (selector == 'combined') {
    summary_df <- tibble(first_tweet, data_gaps, first_last)
  }
  summary_list[['summary_df']] <- summary_df
  return(summary_list)
}


get_tweets_pre_post_data <- function(df_tweets, event_dates, n_days) {
  #' The function takes in a Tweet data frame, a event dates vector, the number
  #' of days to be analyzed in both directions of the event date and returns
  #' the Tweet data classified within the days pre-/post the event and outside of
  #' the n days before and after the event
  assert('The provided data frame is not a Tweet data frame: created_at column is missing', {('created_at' %in% colnames(df_tweets))})
  # filter Tweet data frame for dates at or after the first recorded event
  df_tweets <- filter(df_tweets, (created_at >= min(event_dates) & created_at <= max(event_dates) + days(n_days)))
  df_results <- tibble()
  df_tweets_copy <- df_tweets
  for (day_diff in append(seq(0, 10), seq(-1, -10))) {
    relevant_dates <- ymd(event_dates) + days(day_diff)
    df_results <- rbind(df_results, mutate(filter(df_tweets_copy, created_at %in% relevant_dates), day_difference = as.character(day_diff)))
    df_tweets_copy <- filter(df_tweets_copy, !(created_at %in% relevant_dates))
  }
  df_results <- rbind(df_results, mutate(df_tweets_copy, day_difference = 'outside')) %>%
    group_by(created_at, day_difference) %>%
    dplyr::summarise(number_tweets = n())
  # deal with dates which fall wihtin the range of two categories
  # df_results <- df_results[order(factor(df_results$day_difference, levels = c(as.character(seq(0,n_days)), as.character(seq(-1, -n_days)), 'outside'))),]
  # df_results <- df_results[!duplicated(df_results$created_at),]
  return(df_results)
}

get_tweets_pre_post_data_by_party <- function(df_tweets, event_dates, n_days) {
  #' The function takes in a Tweet data frame, a event dates vector, the number
  #' of days to be analyzed in both directions of the event date and returns
  #' the Tweet data classified within the days pre-/post the event and outside of
  #' the n days before and after the event
  assert('The provided data frame is not a Tweet data frame: created_at column is missing', {('created_at' %in% colnames(df_tweets))})
  # filter Tweet data frame for dates at or after the first recorded event
  df_tweets <- filter(df_tweets, (created_at >= min(event_dates) & created_at <= max(event_dates) + days(n_days)))
  df_results <- tibble()
  df_tweets_copy <- df_tweets
  for (day_diff in append(seq(0, 10), seq(-1, -10))) {
    relevant_dates <- ymd(event_dates) + days(day_diff)
    df_results <- rbind(df_results, mutate(filter(df_tweets_copy, created_at %in% relevant_dates), day_difference = as.character(day_diff)))
    df_tweets_copy <- filter(df_tweets_copy, !(created_at %in% relevant_dates))
  }
  df_results <- rbind(df_results, mutate(df_tweets_copy, day_difference = 'outside')) %>%
    group_by(created_at, day_difference, party_name) %>%
    dplyr::summarise(number_tweets = n())
  # deal with dates which fall wihtin the range of two categories
  # df_results <- df_results[order(factor(df_results$day_difference, levels = c(as.character(seq(0,n_days)), as.character(seq(-1, -n_days)), 'outside'))),]
  # df_results <- df_results[!duplicated(df_results$created_at),]
  return(df_results)
}


get_mean_median_retweet_count <- function(df) {
  df_return <- df %>%
  group_by(first_last) %>%
  summarise(mean_retweets = mean(retweet_count),
            median_retweets = median(retweet_count))
  
  return(df_return)
}

get_mean_median_like_count <- function(df) {
  df_return <- df %>%
    group_by(first_last) %>%
    summarise(mean_likes = mean(favorite_count),
              median_likes = median(favorite_count))
  
  return(df_return)
}
