# ---- Intro ----
#' The purpose of this script is to get Twitter data consisting of the Tweets of
#' Congressmen and Congresswomen and candidates which lost their election.
#' Therefore, I first need to find out the Twitter user names of the defined
#' group. As I get this data based on different methods and from various sources
#' I will first create a data frame which summarizes all the Twitter accounts
#' with the corresponding name of the politician and party as the interpretation
#' in the end will rely on the names of the politician and to account for the
#' fact that politicians might have several Twitter accounts. Afterwards, I will
#' use the Twitter API to get as much data as possible storing the data for each
#' user individually. I decided against storing the data in one big file due to
#' storage capacity. The current setup will allow me to load the data
#' successively from the cloud only keeping the relevant topical Tweets

# load libraries
library(rtweet)
library(tidyverse)
library(stringi)
library(readxl)
library(plyr)

# load helper functions
source("helper_functions.R")

# Twitter API data, does not need to be executed every time, only in the case of changes
# api_app_name <- "PolChange"
# api_key <- "mPZOC2K3j2fdnobtrvOfAeVcG"
# api_key_secret <- "29s5AjV1IYNw5T963XXDMIiZzxPlZWxg5rgPLaSMNvJA6wdeqz"
# api_bearer_token <- "AAAAAAAAAAAAAAAAAAAAANW4jgEAAAAAjnqSebH14SeNo1yRYQR1WM3aHo4%3DCMWUwOnHtYnbnaxrrGIhQbO5VSBkZU2MwJ0H1ZN2T4EZ7eqDup"
# access_token <- "1595707688046022657-KWLJeRDRu860jPAzQSpMG8qRmrh3PV"
# access_token_secret <- "jJLJW2h3WJvjlxZG8m9U0DJI8AOhJqvqi8fwihOkwGSxp"
# 
# token <- create_token(
#   app = api_app_name,
#   consumer_key = api_key,
#   consumer_secret = api_key_secret,
#   access_token = access_token,
#   access_secret = access_token_secret
# )
# 
# auth_setup_default()

# ---- Twitter Accounts Overview ----
# load existing data sets
df_existing_set_1 <- read_csv('Data/Twitter_Handle_Data/active_accounts.csv')
df_existing_set_2 <- read_csv('Data/Twitter_Handle_Data/dataset.csv')
df_existing_set_3 <- read_excel('Data/Twitter_Handle_Data/congress_twitter_117th.xlsx', skip = 1)

# clean existing data frames
df_existing_set_1 <- df_existing_set_1 %>%
  filter(office_title %in% c('House Representative', 'Governor', 'Gubernatorial Candidate', 'House Candidate', 'Senate Candidate', 'Senator')) %>%
  mutate(name = paste(first_name, last_name))
df_existing_set_1$party_name[df_existing_set_1$user_name == 'Kilili4Congress'] <- 'Democrat'
df_existing_set_1$party_name[df_existing_set_1$party_name == 'Independent'] <- 'Democrat'

# remove duplicates
df_existing_set_2 <- df_existing_set_2[!duplicated(df_existing_set_2$Twitter_username),]

# clean party affiliation (dropping foreign politicians and assign right parent party)
df_existing_set_2 <- df_existing_set_2[!df_existing_set_2$Political_party %in% c('Popular Democratic Party', 'New Democratic Party'),]
df_existing_set_2$Political_party[grepl('Democratic|Democrats|Liberal Party of New York|Liberal Union Party', df_existing_set_2$Political_party)] <- 'Democrat'
df_existing_set_2$Political_party[grepl('Republican|Republicano|Populist Party|Constitution Party', df_existing_set_2$Political_party)] <- 'Republican'
df_existing_set_2$Political_party[df_existing_set_2$Name %in% c('Al Gross', 'Angus King', 'Bill Walker', 'Evan McMullin', 'Tim Ashe')] <- 'Democrat'
df_existing_set_2$Political_party[df_existing_set_2$Name %in% c('Ben Carson', 'Ivanka Trump', 'Kenneth Map', 'Pat Buchanan')] <- 'Republican'
df_existing_set_2 <- df_existing_set_2[!df_existing_set_2$Name %in% c('Alexandra Lúgaro', 'David Grosso', 'Elissa Silverman', 'Michael Hayden', 'Robert J. Healey'),]
df_existing_set_2$Political_party[df_existing_set_2$Political_party == 'Libertarian Party'] <- 'Libertarian'
df_existing_set_2$Political_party[df_existing_set_2$Political_party == 'Green Party of the United States'] <- 'Green'
df_existing_set_2 <- df_existing_set_2[df_existing_set_2$Political_party %in% c('Democrat', 'Republican', 'Libertarian', 'Green'),]

# rename columns consistently
colnames(df_existing_set_1)[colnames(df_existing_set_1) %in% c('user_name')] <- 'screen_name'
colnames(df_existing_set_2)[colnames(df_existing_set_2) %in% c('Twitter_username', 'Political_party')] <- c('screen_name', 'party_name')

# extract first and last name
df_existing_set_1 <- split_name(df_existing_set_1, df_existing_set_1$name)

df_existing_set_2 <- split_name(df_existing_set_2, df_existing_set_2$Name)


# get Twitter lists data (current congress members)
df_current_house_tl <- lists_members(list_id = 225745413, n = Inf) %>%
  mutate(office_title = 'House Representative')
df_house_additional_tl <- lists_members(list_id = 63915247, n = Inf) %>%
  mutate(office_title = 'House Representative')
df_current_senate_tl <- lists_members(list_id = 108816487, n = Inf) %>%
  mutate(office_title = 'Senator')
df_senate_additional_tl <- lists_members(list_id = 63915645, n = Inf) %>%
  mutate(office_title = 'Senator')
df_current_governors_tl <- lists_members(list_id = 88692902, n = Inf) %>%
  mutate(office_title = 'Governor')

# combine all data frames derived from Twitter list into one
df_twitter_lists <- rbind(df_current_house_tl, df_house_additional_tl,
                          df_current_senate_tl, df_senate_additional_tl,
                          df_current_governors_tl)

# remove duplicates
df_twitter_lists <- df_twitter_lists[!duplicated(df_twitter_lists),]

# remove individual Twitter list data frames to save working memory
rm(df_current_house_tl, df_house_additional_tl, df_current_senate_tl,
   df_senate_additional_tl, df_current_governors_tl)

# clean name column of Twitter list data frame
df_twitter_lists$name <- str_replace(df_twitter_lists$name, "Congressman ?|Congresswoman ?|Representative ?|Senator ?|Sen\\. ?|Del\\. ?|Congressmember ?|US Rep\\.? ?|Cong\\. ?|Governor |Gov\\. ", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, "Rep\\.? ?| \\(US Sen. ret\\.\\)", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, ", M\\.D\\.|, MD|Dr\\. |, DDS| Ed\\.D", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, "The Office of ?| ?Press Office|Archived: |ARCHIVED: |Archive: |Former ?| ARCHIVE", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, 'Office of ?|\"GT\" ', '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, "U\\.S\\. ?| Office| Press| HQ|Coach | #fella|#DCStatehood |'s", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, " For Congress| for Congress| for US Senate| for NJ", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, " ?\U0001f1fa\U0001f1f8| ?\U0001f1fa\U0001f1e6|\U0001f1e6\U0001f1f8️| \U0001f5f3| \U0001f9ac", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, " for Congress| for US Senate| for NJ", '')
df_twitter_lists$name <- str_replace(df_twitter_lists$name, " ?\U0001f1fa\U0001f1e6", '')
df_twitter_lists$name[df_twitter_lists$screen_name == 'DonJBacon'] <- 'Don J. Bacon'
df_twitter_lists$name[df_twitter_lists$screen_name == 'GT_TeamGT'] <- 'Glenn Thompson'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RalphNortham'] <- 'Ralph Northam'
df_twitter_lists$name[df_twitter_lists$screen_name == 'SenToomey'] <- 'Pat Toomey'
df_twitter_lists$name[df_twitter_lists$screen_name == 'ScottPetersCA50'] <- 'Scott Peters'
df_twitter_lists$name[df_twitter_lists$screen_name == 'CAgovernor'] <- 'Gavin Newsom'
df_twitter_lists$name[df_twitter_lists$screen_name == 'GovHawaii'] <- 'Josh Green'
df_twitter_lists$name[df_twitter_lists$screen_name == 'GovSisolak'] <- 'Steve Sisolak'
df_twitter_lists$name[df_twitter_lists$screen_name == 'InhofePress'] <- 'Jim Inhofe'
df_twitter_lists$name[df_twitter_lists$screen_name == 'CapitoforWV'] <- 'Shelley Moore Capito'
df_twitter_lists$name[df_twitter_lists$screen_name == 'WydenPress'] <- 'Ron Wyden'
df_twitter_lists$name[df_twitter_lists$screen_name == 'GrassleyPress'] <- 'Chuck Grassley'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RoyBluntMO'] <- 'Roy Blunt'
df_twitter_lists$name[df_twitter_lists$screen_name == 'BobMenendezNJ'] <- 'Bob Menendez'
df_twitter_lists$name[df_twitter_lists$screen_name == 'SenCoonsOffice'] <- 'Chris Coons'
df_twitter_lists$name[df_twitter_lists$screen_name == 'LeaderMcConnell'] <- 'Mitch McConnell'
df_twitter_lists$name[df_twitter_lists$screen_name == 'SenMurphyOffice'] <- 'Chris Murphy'
df_twitter_lists$name[df_twitter_lists$screen_name == 'SenHawleyPress'] <- 'Josh Hawley'
df_twitter_lists$name[df_twitter_lists$screen_name == 'SenatorMarshall'] <- 'Roger Marshall'
df_twitter_lists$name[df_twitter_lists$screen_name == 'SenOssoff'] <- 'Jon Ossoff'
df_twitter_lists$name[df_twitter_lists$screen_name == 'senorrinhatch'] <- 'Orrin Hatch'
df_twitter_lists$name[df_twitter_lists$screen_name == 'McConnellPress'] <- 'Mitch McConnell'
df_twitter_lists$name[df_twitter_lists$screen_name == 'tommcclintock'] <- 'Tom McClintock'
df_twitter_lists$name[df_twitter_lists$screen_name == 'millermeeks'] <- 'Mariannette Miller-Meeks'
df_twitter_lists$name[df_twitter_lists$screen_name == 'AyannaPressley'] <- 'Ayanna Soyini Pressley'
df_twitter_lists$name[df_twitter_lists$screen_name == 'ReElectCohen'] <- 'Steve Cohen'
df_twitter_lists$name[df_twitter_lists$screen_name == 'Higgins4WNY'] <- 'Brian Higgins'
df_twitter_lists$name[df_twitter_lists$screen_name == 'TimWalberg'] <- 'Tim Wahlberg'
df_twitter_lists$name[df_twitter_lists$screen_name == 'LangevinForRI'] <- 'James Langevin'
df_twitter_lists$name[df_twitter_lists$screen_name == 'Matsui4Congress'] <- 'Doris Matsui'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RepKarenBass'] <- 'Karen Bass'
df_twitter_lists$name[df_twitter_lists$screen_name == 'Vote4Cleaver'] <- 'Emanuel Cleaver'
df_twitter_lists$name[df_twitter_lists$screen_name == 'GoDutchForMD'] <- 'Dutch Ruppersberger'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RepScottPerry'] <- 'Scott Perry'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RepJerryNadler'] <- 'Jerry Nadler'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RepGallagher'] <- 'Mike Gallagher'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RepPressley'] <- 'Ayanna Soyini Pressley'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RepWaltzPress'] <- 'Mike Waltz'
df_twitter_lists$name[df_twitter_lists$screen_name == 'RepKatCammack'] <- 'Kat Cammack'

# extract first and last name
df_twitter_lists <- split_name(df_twitter_lists, df_twitter_lists$name)

#' add party name to twitter lists data frame by using the name party assignment
#' of the existing data frames
df_existing_party_assignments <- rbind(select(df_existing_set_1, c('party_name', 'first_last')),
                                       select(df_existing_set_2, c('party_name', 'first_last')))
df_existing_party_assignments <- df_existing_party_assignments[!duplicated(df_existing_party_assignments),]

# match first_last names
df_twitter_lists <- merge(df_twitter_lists, df_existing_party_assignments, by = c('first_last'), all.x = TRUE)

# reconcile column names
colnames(df_twitter_lists)[colnames(df_twitter_lists) %in% c('user_id')] <- 'twitter_id'

# reconcile data types
df_existing_set_1$twitter_id = as.character(df_existing_set_1$twitter_id)

#' merge first existing data set and twitter list data frame and add the observations
#' of latter which are not in first one
df_politicians <- merge(df_existing_set_1, 
                        select(df_twitter_lists, -c('name')), 
                        by = c('first_last', 'first_name', 'last_name', 'office_title', 'screen_name', 'twitter_id', 'party_name'), 
                        all.x = TRUE)
df_politicians <- rbind.fill(df_politicians, filter(df_twitter_lists, !screen_name %in% df_politicians$screen_name))
df_politicians <- df_politicians[!duplicated(df_politicians$screen_name), ]

#' manually assign party to NAs
people_unmatched <- filter(df_politicians, is.na(df_politicians$party_name))$name
df_politicians$party_name[df_politicians$name %in% c('Andrew Clyde', 'Andy Ogles', 'Dale W. Strong', 'George Santos', 
                                                         'John Duarte', 'Keith Self', 'Mark Gordon', 'Monica De La Cruz', 
                                                         'Nathaniel Moran', 'Team Meadows', 'Utah Spencer J. Cox')] <- 'Republican'

df_politicians$party_name[df_politicians$name %in% c('Bernie Sanders', 'Delia Ramirez', 'G. K. Butterfield', 'Heidi Heitkamp', 
                                                         'James Langevin', 'JB Pritzker', 'Jeff Jackson', 'Jonathan Jackson', 
                                                         'Sydney Kamlager-Dove', 'Team Katherine Clark')] <- 'Democrat'

#' correct misspelling
df_politicians$name[df_politicians$name == 'Team Meadows'] <- 'Mark Meadows'
df_politicians$name[df_politicians$name == 'Team Katherine Clark'] <- 'Katherine Clark'
df_politicians$name[df_politicians$name == 'James Langevin'] <- 'Jim Langevin'

#' recalibrate first and last name
df_politicians <- split_name(df_politicians, df_politicians$name)

#' load politicians Twitter account data
df_account_overview <- lookup_users(unique(df_politicians$screen_name))

politicians_columns_to_drop <- setdiff(intersect(colnames(df_politicians), colnames(df_account_overview)), 'screen_name')
df_politicians <- df_politicians %>%
  select(-politicians_columns_to_drop)

columns_to_keep <- c('id', 'id_str', 'name', 'screen_name', 'location', 'description', 'protected', 'followers_count', 'friends_count', 'listed_count', 'created_at', 'favourites_count', 'verified', 'default_profile')

df_account_overview <- df_account_overview %>%
  select(columns_to_keep)

df_politicians <- merge(df_politicians, df_account_overview, by = ('screen_name'))

#' save clean version of the data
write_csv(df_politicians, 'Data/Politicians_Data/overview.csv')



# ---- Pull Twitter Data From 
branch_list <- c('senate', 'house', 'governor')

for (branch in branch_list) {
  if (branch == 'senate'){
    #' get right output path
    output_path <- 'Data/Tweet_Data/Senate/'
    
    #' filter politicians set to only include politicians related to branch
    screen_names <- filter(df_politicians, office_title %in% c('Senator', 'Senate Candidate'))$screen_name
  }
  if (branch == 'house'){
    #' get right output path
    output_path <- 'Data/Tweet_Data/House/'
    
    #' filter politicians set to only include politicians related to branch
    screen_names <- filter(df_politicians, office_title %in% c('House Representative', 'House Candidate'))$screen_name
  }
  if (branch == 'governor'){
    #' get right output path
    output_path <- 'Data/Tweet_Data/Governor/'
    
    #' filter politicians set to only include politicians related to branch
    screen_names <- filter(df_politicians, office_title %in% c('Governor', 'Gubernatorial Candidate'))$screen_name
  }
  
  #' access Twitter timeline for all candidates and saving the data in individual
  #' csv files with the screen name used as the file name to guarantee singularity
  for (i in 1:length(screen_names)){
    try(temp1 <- get_timeline(user = screen_names[i], n = Inf, retryonratelimit = TRUE))
    try(temp2 <- get_timeline(user = screen_names[i], n = Inf, max_id = temp1, retryonratelimit = TRUE))
    try(temp <- rbind(temp1, temp2))
    try(temp <- temp[, -which(names(temp) %in% c("coordinates", "retweeted_status", "quoted_status_permalink", "quoted_status", "geo", 'place', "entities", "possibly_sensitive", "truncated", "contributors", "favorited", "favorited_by", "scopes", "display_text_width", "quote_count", "timestamp_ms", "reply_count", "filter_level", "metadata", "query", "withheld_scope", "withheld_copyright", "withheld_in_countries", "possibly_sensitive_appealable"))])
    # temp$created_at <- as.character(temp$created_at)
    if (nrow(temp) > 0){
      write.csv(temp, paste(output_path, screen_names[i], ".csv", sep = ''))
    }
  }
}



# ---- Set Variable Parameters ----
#' choose the election type
#' 'house': load election data for US House of Representatives Election
#' 'senate': load election data for US Senate Election
election_type <- 'house'
#' chose the year of the earliest election to get candidate data for
start_year <- 2010
#' share of vote threshold to be included in analysis
min_vote_share <- 0.05


# ---- Load and Clean Data ----
#' get right path
path_to_election_data <- ifelse(election_type == 'senate',
                                'Data/Election_Data/1976-2020-senate.csv',
                                'Data/Election_Data/1976-2020-house.csv')

#' load election data to data frame
df_election <- read_csv(path_to_election_data)

#' filter out data from before 2010 due to Twitter only starting in 2006 and
#' remove uncontested candidates (candidates with no challenger)
#' remove unidentifiaable candidates (writeins, others)
df_election <- df_election %>%
  filter(year >= start_year) %>%
  filter(candidatevotes != totalvotes) %>%
  filter(str_count(candidate, '\\w+') > 1) %>%
  filter(candidatevotes / totalvotes > 0.05) %>%
  mutate(vote_share = candidatevotes / totalvotes)

#' extract candidate names
candidate_names <- str_to_title(unique(df_election$candidate))

#' extract first and last name only
#' 
first_last_names <- as.data.frame(str_split_fixed(candidate_names, " ", -1))
first_last_names <- first_last_names %>%
  filter(V1 != 'Writein') %>%
  filter(V2 != '')

first_names <- first_last_names
for (i in (2:ncol(first_names))) {
  for (j in 1:nrow(first_names)) {
    if (str_length(first_names[j, 1]) <= 1) {
      first_names[j, 1] <- first_names[j, i]
    }
  }
}
first_names$V1 <- gsub('"', '', first_names$V1)

last_names <- first_last_names
for (i in rev(2:ncol(last_names))) {
  for (j in 1:nrow(last_names)) {
    if (!last_names[j, i] %in% c("Jr.", "", "Iii", "Sr.", "Jr", "Sr")) {
      last_names[j, i-1] <- last_names[j, i]
    }
  }
}
first_last_names <- as.data.frame(cbind(as.data.frame(candidate_names), first_names[, 1], last_names[, 1]))
colnames(first_last_names) <- c('name', 'first_name', 'last_name')
first_last_names <- first_last_names %>%
  mutate(first_last = paste(first_name, last_name, sep = ' '))

#' load Twitter handle dataset
df_twitter_handles <- read_csv('Data/Twitter_Handle_Data/active_accounts.csv')



#' Section to get an extensive dataframe where to every election there are the according Twitter accounts
df_election$candidate <- str_to_title(df_election$candidate)
first_last_names_copy <- first_last_names
colnames(first_last_names_copy)[1] <- 'candidate'
election_extensive_df <- merge(first_last_names_copy, df_election)
election_extensive_df <- merge(election_extensive_df, 
                               df_twitter_handles, 
                               by = c('first_name', 'last_name'), 
                               all = TRUE)
election_extensive_df <- election_extensive_df %>%
  mutate(state_po = ifelse(is.na(state_po), state.y, state_po)) %>%
  mutate(district.x = ifelse(is.na(district.x), district.y, district.x)) %>%
  mutate(party = ifelse(is.na(party), toupper(party_name), party))

election_extensive_df <- select(election_extensive_df, -c(state.y, district.y, party_name, writein))

colnames(election_extensive_df)[colnames(election_extensive_df) %in% c('state.x', 'district.x', 'user_name')] <- c('state', 'district', 'screen_name')

output_path_ext <- ifelse(election_type == 'senate',
                          'Data/Election_Data/since-2010-senate.csv',
                          'Data/Election_Data/since-2010-house.csv')

write_csv(election_extensive_df, output_path_ext)


#' match names with Twitter handles
matched <- merge(first_last_names, df_twitter_handles)

#' record unmatched candidates
unmatched <- merge(first_last_names, df_twitter_handles, all.x = TRUE) %>%
  filter(is.na(twitter_id))

matched_names <- unique(matched$user_name)

congress_data_1 <- read_csv('Data/Harvard_Datasets/115th Congress/house.csv')
congress_data_2 <- read_csv('Data/Harvard_Datasets/116th Congress/house.csv')

load('missing.rda')

additional_names <- union(unique(congress_data_1$user_screen_name), unique(congress_data_2$user_screen_name))
additional_names <- setdiff(additional_names, df_politicians$screen_name)
party_name_append <- c('Democrat', 'Democrat', 'Republican', 'Democrat', 'Republican', 
                       'Republican', 'Democrat', 'Democrat', 'Republican', 'Democrat',
                       'Democrat', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Republican', 'Republican')
first_last_append <- c('Doug Jones', 'Kamala Harris', 'Luther Strange', 'Claire Mccaskill',
                       'Marco Rubio', 'David Perdue', 'Al Franken', 'Heidi Heitkamp',
                       'Jon Kyl', 'Tom Udall', 'Doug Jones', 'Martha Mcsally',
                       'Martha Mcsally', 'Pat Roberts', 'Johnny Isakson',
                       'Cory Gardner', 'Kelly Loeffler')
office_title_append <- c('Senator', 'Vice President', 'Senator', 'Senator', 
                         'Senator', 'Senator', 'Senator', 'Senator', 'Senator', 
                         'Senator', 'Senator', 'Senator', 'Senator', 'Senator', 
                         'Senator', 'Senator', 'Senator')
state_append <- c('AL', 'CA', 'AL', 'MO', 'FL', 'GA', 'MN', 'ND', 'AZ', 'NM', 'AL',
                  'AZ', 'AZ', 'KS', 'GA', 'CO', 'GA')

df_to_append <- tibble('first_last' = first_last_append,
                       'office_title' = office_title_append,
                       'screen_name' = additional_names,
                       'party_name' = party_name_append,
                       'state' = state_append)

df_politicians <- rbind.fill(df_politicians, df_to_append)


# ---- Access and Save Data Through API ----
#' get right output path
output_path <- ifelse(election_type == 'senate',
                      'Data/Tweet_Data/Senate/',
                      'Data/Tweet_Data/House/')

#' access Twitter timeline for all candidates and saving the data in individual
#' csv files with the screen name used as the file name to guarantee singularity
for (i in 1:length(additional_names)){
  try(temp1 <- get_timeline(user = additional_names[i], n = Inf, retryonratelimit = TRUE))
  try(temp2 <- get_timeline(user = additional_names[i], n = Inf, max_id = temp1, retryonratelimit = TRUE))
  try(temp <- rbind(temp1, temp2))
  try(temp <- temp[, -which(names(temp) %in% c("coordinates", "retweeted_status", "quoted_status_permalink", "quoted_status", "geo", 'place', "entities", "possibly_sensitive", "truncated", "contributors", "favorited", "favorited_by", "scopes", "display_text_width", "quote_count", "timestamp_ms", "reply_count", "filter_level", "metadata", "query", "withheld_scope", "withheld_copyright", "withheld_in_countries", "possibly_sensitive_appealable"))])
    # temp$created_at <- as.character(temp$created_at)
  if (nrow(temp) > 0){
    write.csv(temp, paste(output_path, additional_names[i], ".csv", sep = ''))
  }
}
