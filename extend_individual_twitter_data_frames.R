#' The purpose of this script is to extend the Twitter Data with the Data coming
#' from the Harvard Dataverse Tweet data. Thus, it is important that we have
#' matching columns before appending the data to the existing data. To do so I
#' remove irrelevant columns and rename other columns containing the same
#' information under different names.

library(tidyverse)

source("helper_functions.R")

create_combined_df <- function(existing_data_df, additional_data_df){
  #' extends both data frames with columns filled with empty values that exist
  #' in the other data frame respectively to make the data match-able
  #' afterwards, merge two dataframes to one
  #' 
  #' remove unnecessary columns which can lead to problems
  columns_to_be_removed = c('X1', 'coordinates', 'place', 'possibly_sensitive', 
                            'user_default_profile_image', 'user_description', 
                            'user_listed_count', 'user_location', 'user_time_zone',
                            'user_created_at', 'id_str', 'quoted_status_id_str',
                            'full_text', 'display_text_range', 
                            "in_reply_to_status_id_str", "in_reply_to_user_id_str")
  existing_data_df <- select(existing_data_df, -intersect(colnames(existing_data_df), columns_to_be_removed))
  additional_data_df <- select(additional_data_df, -intersect(colnames(additional_data_df), columns_to_be_removed))
  
  #' find missing columns for each data frame
  col_exist_missing <- setdiff(colnames(additional_data_df), colnames(existing_data_df))
  col_add_missing <- setdiff(colnames(existing_data_df), colnames(additional_data_df))
  
  #' find constant columns which have the same value for all observations
  constants_additional <- c()
  for (i in 1:ncol(additional_data_df)){
    if (length(unique(additional_data_df[[i]])) == 1){
      constants_additional <- append(constants_additional, colnames(additional_data_df)[i])
    }
  }
  
  constants_existing <- c()
  for (i in 1:ncol(existing_data_df)){
    if (length(unique(existing_data_df[[i]])) == 1){
      constants_existing <- append(constants_existing, colnames(existing_data_df)[i])
    }
  }
  
  if (length(col_exist_missing) != 0) {
    for (i in 1:length(col_exist_missing)){
      existing_data_df[col_exist_missing[i]] <- ifelse(col_exist_missing[i] %in% constants_additional, 
                                               select(additional_data_df, col_exist_missing[i])[[1,1]], 
                                               NA)
    }
  }
  
  if (length(col_add_missing) != 0) {
    for (i in 1:length(col_add_missing)){
      additional_data_df[col_add_missing[i]] <- ifelse(col_add_missing[i] %in% constants_existing, 
                                                       select(existing_data_df, col_add_missing[i])[[1,1]], 
                                                       NA)
    }
  }
  
  if (str_count(additional_data_df$created_at[1], '\\w+') == 8){
    additional_data_df$created_at <- sapply(additional_data_df$created_at, to_date)
    additional_data_df$created_at <- lubridate::ymd_hms(additional_data_df$created_at)
  }
  
  #' merge data frames, remove duplicates and return ordered by date
  merged_df <- rbind(existing_data_df, additional_data_df)
  merged_df <- filter(merged_df, duplicated(text) == FALSE)
  return(merged_df[order(merged_df$created_at),])
}


congress_data <- read_csv('Data/Harvard_Datasets/116th Congress/house.csv')
congress_data2 <- read_csv('Data/Harvard_Datasets/115th Congress/house.csv')
congress_data3 <- read_csv('Data/Harvard_Datasets/115th Congress/senators.csv')
congress_data4 <- read_csv('Data/Harvard_Datasets/116th Congress/senate.csv')

congress_data <- rbind(congress_data, congress_data2, congress_data3, congress_data4)

unique_names <- unique(congress_data$user_screen_name)

missing_existing_names <- c()

for (i in 1:length(unique_names)){
  path <- paste('Data/Tweet_Data/House/', unique_names[i], '.csv', sep = '')
  existing_data_df <- try(read_csv(path))
  additional_data_df <- filter(congress_data, user_screen_name == unique_names[i])
  if (class(existing_data_df) == "try-error"){
    missing_existing_names <- append(missing_existing_names, unique_names[i])
    next
  }
  else {
    new_df <- create_combined_df(existing_data_df, additional_data_df)
  }
  write_csv(new_df, path)
}
