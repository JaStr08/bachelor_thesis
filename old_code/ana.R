library(tidyverse)
library(rtweet)

# ids_climate1 <- as.numeric(scan("climate_id.txt.00", character(), quote = ''))
# 
# climate <- tibble()
# 
# for (i in 1:1000){
#   cl_temp <- lookup_tweets(ids_climate1[(i-1)*10000+1:10000*i])
#   climate <- rbind(climate, cl_temp)
#   save_cl <- climate[, -which(names(climate) %in% c("coordinates", "retweeted_status", "quoted_status_permalink", "quoted_status", "geo", 'place', "entities", "possibly_sensitive", "truncated", "contributors", "favorited", "favorited_by", "scopes", "display_text_width", "quote_count", "timestamp_ms", "reply_count", "filter_level", "metadata", "query", "withheld_scope", "withheld_copyright", "withheld_in_countries", "possibly_sensitive_appealable"))]
#   write.csv(save_cl, "climate.csv")
#   print(i)
# }




test <- read_csv("senators.csv")
print(head(test))

test %>%
  group_by(user_screen_name) %>%
  summarise(n = n())

drop <- c("coordinates", "possibly_sensitive", "user_default_profile_image", "lang")
test <- select(test, -drop)

test <- mutate(test, year_month = format(
  as.Date(paste(str_sub(test$created_at, 5, 7), str_sub(test$created_at, 9, 10), 
                str_sub(test$created_at, 27, 30), sep = "-"), 
          "%b-%d-%Y"), "%Y-%m"))

test_ak <- filter(test, user_screen_name == "amyklobuchar")

to_plot <- test_ak %>%
  group_by(year_month) %>%
  summarise(n = n())

ggplot(to_plot, aes(as.Date(paste(year_month, "-01", sep = "")), n)) +
  geom_line()

test_mult <- filter(test, user_screen_name %in% c("amyklobuchar", "BillCassidy", "ChrisCoons", "ChrisVanHollen", "ChuckGrassley", "DougJones"))

to_plot <- test_mult %>%
  group_by(year_month, user_screen_name) %>%
  summarise(n = n())

ggplot(to_plot, aes(as.Date(paste(year_month, "-01", sep = "")), n, color = user_screen_name)) +
  geom_line()

climate_data <- filter(test, grepl("climate change|climate|environment|global warming|climate agreement|\\<epa\\>|paris agreement|pollution|ozone", tolower(test[["text"]])))

# Hurricane Irma
climate_data <- filter(test, (grepl("\\<irma\\>|hurricane irma|storm|hurricane|wildfire|flooding", tolower(test[["text"]])) & grepl("climate change|climate|environment|global warming|climate agreement|\\<epa\\>|paris agreement|pollution|ozone", tolower(test[["text"]]))))

climate_mult <- filter(climate_data, user_screen_name %in% c("amyklobuchar", "BillCassidy", "ChrisVanHollen", "ChuckGrassley", "DougJones"))

to_plot_cl <- climate_mult %>%
  group_by(year_month, user_screen_name) %>%
  summarise(n = n())

ggplot(to_plot_cl, aes(as.Date(paste(year_month, "-01", sep = "")), n, color = user_screen_name)) +
  geom_line()
