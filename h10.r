# 296 final project

# libraries i need + annoying startup messages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(paletteer))

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


# some more useful information for our graphs and analysis

# average overall popularity change for individual languages
lang_avg <- processed_data %>% 
  group_by(language, era) %>%
  summarise(avg_popularity = mean(popularity), .groups = "drop")

# average popularity change as the years progress for each era
era_yearly <- processed_data %>%
  group_by(era, year) %>%
  summarise(avg_popularity = mean(popularity), .groups = "drop") %>%
  group_by(era) %>%
  mutate(yoy_change = avg_popularity - lag(avg_popularity))


# alright, now we will need data to attempt to answer our questions

# Do newer languages show a consistent growth trend while older languages slip into irrelevancy?
growth_model <- lm(popularity ~ release_year, data = processed_data)
summary(growth_model)

# Are there any older languages that are outperforming expectations?


# Can we forecast what languages are likely to grow or shrink in usage over the next couple of years?


# we are going to need a couple of graphs to fully vizualize and understand everything that is going on

# plot 1 - overall language trends over time
ggplot(processed_data, aes(x = year, y = popularity, group = language, color = era)) +
  geom_line(linewidth = 1) +
  scale_color_paletteer_d("nationalparkcolors::Arches")

# plot 2 - honed in average trend per era over time
ggplot(era_yearly, aes(x = year, y = avg_popularity, color = era)) +
  geom_line(linewidth = 1) +
  scale_color_paletteer_d("nationalparkcolors::Arches")

# plot 3 - this second plot will help show each languages popularity performance as time goes on. we can cross compare them individually
ggplot(processed_data, aes(x = year, y = popularity, color = era)) +
  geom_line() +
  facet_wrap(~ language, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # this simply rotates the text on the graph
  scale_color_paletteer_d("nationalparkcolors::Arches") 
  
# plot 4 - average popularity per language
ggplot(lang_avg, aes(x = reorder(language, avg_popularity), y = avg_popularity, fill = era)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "language") +
  scale_fill_paletteer_d("nationalparkcolors::Arches")