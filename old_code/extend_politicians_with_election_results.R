rm(list=ls())

library(tidyverse)

source('helper_functions.R')

# politician data set
df_politicians <- read_csv('Data/Politicians_Data/overview.csv')

# house election data set
df_elections_house <- read_csv('Data/Election_Data/1976-2020-house.csv')

# senate elction data set
df_elections_senate <- read_csv('Data/Election_Data/1976-2020-senate.csv')

# filter for elections 2010 or later
df_elections_house <- df_elections_house %>%
  filter(year >= 2010)

df_elections_senate <- df_elections_senate %>%
  filter(year >= 2010)

# get first_last name column for election data
df_elections_house <- split_name(df_elections_house, str_to_title(df_elections_house$candidate))
df_elections_senate <- split_name(df_elections_senate, str_to_title(df_elections_senate$candidate))

# get vote share
df_elections_house <- df_elections_house %>%
  mutate(share = candidatevotes / totalvotes)

df_elections_senate <- df_elections_senate %>%
  mutate(share = candidatevotes / totalvotes)

# identify winning candidate for each election
df_elections_house <- df_elections_house %>%
  group_by(year, state, district) %>%
  mutate(elected = ifelse(share == max(share), T, F))

df_elections_senate <- df_elections_senate %>%
  group_by(year, state, totalvotes) %>%
  mutate(elected = ifelse(share == max(share), T, F))

# get election years %>%
house_election_years <- unique(df_elections_house$year)

# identify incumbent
identify_incumbent <- function(df, x) {
  df <- mutate(df, incumbent = F)
  for (name in df[['first_last']]) {
    filtered_df <- filter(df, first_last == name)
    for (year in unique(filtered_df$year)) {
      single_election_df <- filter(filtered_df, year == year)
      if (T %in% single_election_df$elected){
        df[df$year == year+x & df$first_last == name, 'incumbent'] <- T
      }
    }
  }
  return(df)
}

df_elections_house <- identify_incumbent(df_elections_house, 2)
df_elections_senate <- identify_incumbent(df_elections_senate, 6)

df_elections_house <- df_elections_house %>%
  filter(year >= 2010)

df_elections_senate <- df_elections_senate %>%
  filter(year >= 2010)

df_politicians_election <- rbind(df_elections_house, df_elections_senate) %>%
  ungroup() %>%
  select(c('year', 'state_po', 'stage', 'special', 'candidate', 'writein', 'candidatevotes', 'totalvotes', 'first_last', 'share', 'elected', 'incumbent'))
colnames(df_politicians_election)[colnames(df_politicians_election) %in% c('state_po')] <- c('state_po')

df_politicians_election <- merge(select(df_politicians, -c('state')), df_politicians_election, by = 'first_last')

# save extended data frame
write_csv(df_politicians_election, 'Data/Politicians_Data/elections_without_governors.csv')

test <- tibble('A' = c(1, 6, 4),
               'B' = c(3, 2, 6))

do_function <- function() {
  browser()
  mutate(test, sum = A + B)
}

do_function()
