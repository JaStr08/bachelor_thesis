rm(list = ls())

library(tidyverse)

total <- c()
covid <- c()

paths <- c("press_release_data/tammy_baldwin.csv", 
           "press_release_data/cory_booker.csv",
           "press_release_data/john_barrasso.csv",
           "press_release_data/john_boozman.csv",
           "press_release_data/marsha_blackburn.csv",
           "press_release_data/michael_bennet.csv",
           "press_release_data/richard_blumenthal.csv",
           "press_release_data/mike_braun.csv",
           "press_release_data/sherrod_brown.csv")

# read press release data
for (path in paths) {
  pr <- read_csv(paste(path))
  pr[["content"]] <- pr[["content"]] %>%
    tolower()
  total <- append(total, nrow(pr))
  
  press_covid <- pr[["content"]][grepl("covid|covid19|covid-19|corona", pr[["content"]])]
  covid <- append(covid, length(press_covid))
}

term_length_days <- c(6*365+7, 10*365-294, 16*365-166, 12*365+7, 4*365, 12*365, 1467, 14*365+7, 16*365)

# pr <- read_csv("press_release_data/tammy_baldwin.csv")
# pr <- read_csv("press_release_data/cory_booker.csv")
# pr <- read_csv("press_release_data/john_barrasso.csv")
# pr <- read_csv("press_release_data/john_boozman.csv")
# pr <- read_csv("press_release_data/marsha_blackburn.csv")
# pr <- read_csv("press_release_data/michael_bennet.csv")
# pr <- read_csv("press_release_data/richard_blumenthal.csv")
# pr <- read_csv("press_release_data/mike_braun.csv")
# pr <- read_csv("press_release_data/sherrod_brown.csv")

# initial cleaning
# pr[["content"]] <- pr[["content"]] %>%
#   tolower() %>%
#   gsub(" ?http(s?)(://)(.*)[.|/](.*)", "", .) %>%
#   gsub("/n", "", .) %>%
#   gsub("/t", "", .)
# 
# total <- append(total, nrow(pr))
# 
# 
# press_climate_change <- pr[["content"]][grepl("climate change", pr[["content"]], fixed = TRUE)]
# press_covid <- pr[["content"]][grepl("covid|covid19|covid-19|corona", pr[["content"]])]
# 
# covid <- append(covid, length(press_covid))

# write_csv(tibble("content" = press_covid), "./press_release_data/covid_test.csv")