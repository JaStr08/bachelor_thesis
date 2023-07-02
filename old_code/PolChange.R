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
twitter_accounts <- list()
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
  twitter_accounts[[candidate_name]] <- filter(temp, followers_count >= 1500)
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


candidates_with_identifiable_twitter <- names(twitter_accounts)[sapply(twitter_accounts, function(x) nrow(x) != 0)]
candidates_no_identifiable_twitter <- names(twitter_accounts)[sapply(twitter_accounts, function(x) nrow(x) == 0)]

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

id, name, screen_name