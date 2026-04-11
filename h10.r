# 296 final project

# libraries i need + annoying startup messages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(scales))

# C# has a pound, ensure the disabling of comments
lang_data <- read.table("languages.csv", header = TRUE, sep = ",", comment.char = "")

# for my testing, the data was in wide format. to convert it to long, we used melt and set the variable to the language, with the value being the property. that date was the id, and tells melt to leave the date alone
# we can pipe this result into a mutate too, to simply delete the month off of the data se we only have 20 entries per language
processed_data <- lang_data %>% melt(id.vars = "Date", variable.name = "language", value.name = "popularity") %>% mutate(year = as.integer(sub(".* ", "", Date)))

# we now needed to group each language and its respective year and summarize the popularity for each. (.group gets rid of warning message)
processed_data %<>% group_by(language, year) %>% summarise(popularity = mean(popularity), .groups = "drop")

# data for release date is sourced from wikipedias "first appeared" definition since some languages became popular before 1.0 release. note that delphi uses 1995 since it seems that was what the data measured
release_years <- c(
  "Python" = 1991, "Java" = 1995, "JavaScript" = 1995,
  "C.C.." = 1972, "Ruby" = 1995, "PHP" = 1995,
  "Swift" = 2014, "Kotlin" = 2011, "Rust" = 2012,
  "Go" = 2009, "TypeScript" = 2012, "R" = 1993,
  "Scala" = 2004, "Dart" = 2011, "Julia" = 2012,
  "Haskell" = 1990, "Lua" = 1993, "Perl" = 1987,
  "C." = 2000, "Groovy" = 2003, "Matlab" = 1984,
  "VBA" = 1993, "Powershell" = 2006, "Cobol" = 1960,
  "Ada" = 1980, "Abap" = 1983, "Visual.Basic" = 1991,
  "Delphi.Pascal" = 1995, "Objective.C" = 1984
)
# put that data in a data frame
release_data <- data.frame(language = names(release_years), release_year = unname(release_years))

# merge our popularity measures with release year
processed_data <- merge(processed_data, release_data, by = "language")

# i can also add general era categories so that we can see the general trend of these languages. i will do this by using the case_when() which lets me chain if else statements
processed_data %<>% mutate(era = case_when(release_year < 1990 ~ "Legacy", release_year < 2006 ~ "Established", TRUE ~ "Modern"))

# this first plot will help show results across the era
ggplot(processed_data, aes(x = year, y = popularity, group = language, color = era)) +
    geom_line()


head(processed_data)
