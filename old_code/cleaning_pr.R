
# filter out all non-English words
library(qdap)
# library(qdapDictionaries)

press_covid_filtered <- intersect(press_covid, GradyAugmented)
frequent_terms <- freq_terms(press_covid, 3)

gsub(" ?http(s?)(://)(.*)[.|/](.*)", "", press_covid[2]) # cuts everything off after and including the first hyperlink
press_covid_cleaned <- press_covid %>%
  gsub("\r", "", .) %>%
  gsub("\n", "", .) %>%
  gsub("\t", " ", .) %>%
  gsub("facebook  twitter  email  print(.*)", "", .)

df_train <- tibble("statements" = press_covid_cleaned,
                  "label" = NA*length(press_covid_cleaned))

readLines(press_covid, 5)


sample1_1 <- press_covid[1]
sample1_2 <- press_covid[2]

sample2_1 <- press_covid[1]
sample2_2 <- press_covid[2]

sample3_1 <- press_covid[1]
sample3_2 <- press_covid[2]

sample4_1 <- press_covid[1]
sample4_2 <- press_covid[2]

sample5_1 <- press_covid[1]
sample5_2 <- press_covid[2]

sample6_1 <- press_covid[1]
sample6_2 <- press_covid[2]

sample7_1 <- press_covid[1]
sample7_2 <- press_covid[2]

sample8_1 <- press_covid[1]
sample8_2 <- press_covid[2]

sample9_1 <- press_covid[1]
sample9_2 <- press_covid[2]