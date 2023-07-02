library(rtweet)
library(tidyverse)
library(lubridate)
# library(here)
# library(igraph)

senate_elections <- read_csv("dataverse_files/1976-2020-senate.csv")
senate_elections <- mutate(senate_elections, perc = candidatevotes / totalvotes)
senate_elections %>%
  group_by(party_simplified) %>%
  summarize("max" = max(perc),
            "min" = min(perc))

# filter out data from before 2010 due to Twitter only starting in 2006
senate_elections <- filter(senate_elections, year >= 2010)

# removing uncontested candidates
senate_elections <- senate_elections %>%
  filter(candidatevotes != totalvotes) %>%
  filter(candidate != "JEFF SESSION")

# removing independent candidates and candidates from other minor parties
# receiving less than 5 percent of votes
temp_ana <- filter(
  senate_elections, 
  party_simplified %in% c("OTHER", "LIBERTARIAN") & perc >= 0.05)

senate_elections <- filter(
  senate_elections, 
  !(party_simplified %in% c("OTHER", "LIBERTARIAN")) | perc >= 0.05)

# removing candidates receiving less than 5 percent of votes
temp_ana2 <- filter(senate_elections, perc < 0.05)

senate_elections <- filter(senate_elections, perc >= 0.05)

# get names of candidate
senate_candidates <- unique(senate_elections$candidate)

# extract possible twitter handles
# n = 2, cause incumbents have two twitter account, one private and the other
# on their official Senator Twitter account
twitter_accounts_1 <- list()
for (candidate_name in senate_candidates) {
  # if (filter(senate_elections, candidate == candidate_name)[[1, 12]] == "DEMOCRAT") {
  #   temp <- search_users(paste(candidate, "Democrat"), n = 2)
  # }
  # if (filter(senate_elections, candidate == candidate)[1:12] == "REPUBLICAN") {
  #   temp <- search_users(paste(candidate, "Republican"), n = 2)
  # }
  # else {
  temp <- search_users(candidate_name, n = 1)
  # }
  twitter_accounts_1[[candidate_name]] <- filter(temp, followers_count >= 1500)
}

twitter_accounts_senate <- list()
for (candidate_name in senate_candidates) {
  # if (filter(senate_elections, candidate == candidate_name)[[1, 12]] == "DEMOCRAT") {
  #   temp <- search_users(paste(candidate, "Democrat"), n = 2)
  # }
  # if (filter(senate_elections, candidate == candidate)[1:12] == "REPUBLICAN") {
  #   temp <- search_users(paste(candidate, "Republican"), n = 2)
  # }
  # else {
  temp <- search_users(paste(candidate_name, "senate"), n = 1)
  # }
  twitter_accounts_senate[[candidate_name]] <- filter(temp, followers_count >= 250)
}

candidates_no_identifiable_twitter2 <- names(twitter_accounts_senate)[sapply(twitter_accounts_senate, function(x) nrow(x) == 0)]


candidates_with_identifiable_twitter <- names(twitter_accounts_1)[sapply(twitter_accounts_1, function(x) nrow(x) != 0)]
candidates_no_identifiable_twitter <- names(twitter_accounts_1)[sapply(twitter_accounts_1, function(x) nrow(x) == 0)]

twitter_accounts2 <- list()
for (candidate_name in candidates_no_identifiable_twitter) {
  # if (filter(senate_elections, candidate == candidate_name)[[1, 12]] == "DEMOCRAT") {
  #   temp <- search_users(paste(candidate, "Democrat"), n = 2)
  # }
  # if (filter(senate_elections, candidate == candidate)[1:12] == "REPUBLICAN") {
  #   temp <- search_users(paste(candidate, "Republican"), n = 2)
  # }
  # else {
  temp <- search_users(candidate_name, n = 1)
  # }
  twitter_accounts2[[candidate_name]] <- filter(temp, followers_count >= 1000)
}

candidates_no_identifiable_twitter <- names(twitter_accounts2)[sapply(twitter_accounts2, function(x) nrow(x) == 0)]

candidates_manual <- intersect(candidates_no_identifiable_twitter, candidates_no_identifiable_twitter2)

extract_screen_names <- function(input_list, output_vector){
  for (entry in input_list){
    output_vector <- append(output_vector, entry[['screen_name']])
  }
}

for (entry in twitter_accounts_senate){
  tempo3 <- append(tempo3, entry[['screen_name']])
}

test <- c()
append(test, c(4, 2))

tempo1 <- c()
tempo2 <- c()
tempo3 <- c()

extract_screen_names(twitter_accounts_1, tempo1)
extract_screen_names(twitter_accounts2, tempo2)
extract_screen_names(twitter_accounts_senate, tempo3)

all_twitter_names <- union(tempo1, union(tempo2, tempo3))

for (name in all_twitter_names[606:length(all_twitter_names)]){
  temp <- get_timeline(user = name, n = Inf, retryonratelimit = TRUE)
  temp2 <- get_timeline(user = name, n = Inf, max_id = temp, retryonratelimit = TRUE)
  temp <- rbind(temp, temp2)
  temp <- temp[, -which(names(temp) %in% c("coordinates", "retweeted_status", "quoted_status_permalink", "quoted_status", "geo", 'place', "entities", "possibly_sensitive", "truncated", "contributors", "favorited", "favorited_by", "scopes", "display_text_width", "quote_count", "timestamp_ms", "reply_count", "filter_level", "metadata", "query", "withheld_scope", "withheld_copyright", "withheld_in_countries", "possibly_sensitive_appealable"))]
  # temp$created_at <- as.character(temp$created_at)
  write.csv(temp, paste("Twitter_Sen/", name, ".csv", sep = ''))
}

for (i in 1:ncol(temp)){
  write.csv(temp[, 1:i], paste("Twitter_Sen/", name, ".csv", sep = ''))
}


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

marcorubio_raw <- get_timeline(user = "GovRonDeSantis", n = Inf, retryonratelimit = TRUE)
marcorubio2 <- get_timeline(user = "GovRonDeSantis", n = Inf, max_id = marcorubio_raw, retryonratelimit = TRUE)

marcorubio <- marcorubio_raw[,]

marcorubio$full_text <- marcorubio$full_text %>%
  gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", .) %>%
  gsub("&amp;", "and", .) %>%
  gsub("co/\\S+\\b", "", .) %>%
  gsub("\n\n", ". ", .) %>%
  gsub("\n", ". ", .)

marcorubio_climate_chnage <- filter(marcorubio, grepl("climate change", tolower(marcorubio$full_text), fixed = TRUE))
marcorubio_covid <- filter(marcorubio, grepl("covid|covid19|covid-19|corona", tolower(marcorubio$full_text)))

set.seed(3245)
index_train <- sample.int(nrow(marcorubio_covid), round(0.2*nrow(marcorubio_covid)))
mr_covid_train <- select(marcorubio_covid[index_train, ], full_text)
mr_covid_pred <- select(marcorubio_covid[-index_train, ], full_text)


marcorubio_vaccine <- filter(marcorubio_covid, grepl("vaccine|vaccination|vaccinated|shot", tolower(marcorubio_covid$full_text)))

set.seed(3245)
index_train <- sample.int(nrow(marcorubio_vaccine), round(0.5*nrow(marcorubio_vaccine)))
mr_vaccine_train <- select(marcorubio_vaccine[index_train, ], full_text)
mr_vaccine_pred <- select(marcorubio_vaccine[-index_train, ], full_text)

# test <- get_timeline(user = "realDonaldTrump", n = 15, parse = TRUE)
# tmls <- get_timeline("_R_Foundation", n = 10)
# 
# rstats <- search_tweets("#rstats", n = 10)
# 
# auth_get()
# auth_as()

# transform senator names to matchable version
last_names <- tolower(str_split_fixed(senate_candidates, " ", -1))
for (i in rev(2:ncol(last_names))) {
  for (j in 1:nrow(last_names)) {
    if (!last_names[j, i] %in% c("jr.", "", "iii", "sr.", "jr", "sr")) {
      last_names[j, i-1] <- last_names[j, i]
    }
  }
}
last_names <- last_names[, 1]
last_names <- gsub(",", "", last_names)

twitter_accounts_senate_twitter
for (name in last_names) {
  tweetcongress_friends[grepl("rou", tweetcongress_friends$name),]
}





tweetcongress <- get_friends("SenateDems")
tweetcongress_friends <- lookup_users(tweetcongress$to_id)
tweetcongress_friends$name <- tolower(tweetcongress_friends$name)

# id, name, screen_name

ids_January_6 <- as.numeric(scan("Datasets Harvard/January 6/Trump-rally-ids.txt", character(), quote = ''))

january6 <- lookup_tweets(ids_January_6, retryonratelimit = TRUE)
january6 <- tibble()

for (i in 4899:17210){
  jan_temp <- lookup_tweets(ids_January_6[(i-1)*87+1:87*i])
  january6 <- rbind(january6, jan_temp)
  print(i)
}
