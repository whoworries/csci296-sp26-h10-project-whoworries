# 296 final project

# libraries i need + annoying startup messages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(paletteer))
library(svglite)

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


# alright, now we will need data to attempt to answer our questions
# we are going to need a couple of graphs to fully vizualize and understand everything that is going on as well

# -------------------------------------------------------------------------------------------------
# P1-2: overall language trends over time
# -------------------------------------------------------------------------------------------------

# average overall popularity change for individual languages. (we include era here to ensure graph can use it)
lang_avg <- processed_data %>% 
  group_by(language, era) %>%
  summarise(avg_popularity = mean(popularity), .groups = "drop")

# this helps quantify what we are looking at
overall_bar <- ggplot(lang_avg, aes(x = reorder(language, avg_popularity), y = avg_popularity, fill = era)) +
  geom_col() +
  coord_flip() +
  labs(x = "language", y = "avg popularity (%)", title = "language avg popularity") +
  scale_fill_paletteer_d("nationalparkcolors::Arches")

# note: not very useful to look at to analyze data, but gives a good intro graph towards the sense of scale
overall_line <- ggplot(processed_data, aes(x = year, y = popularity, group = language, color = era)) +
  geom_line(linewidth = 1) +
  labs(title = "language popularity over time") +
  scale_color_paletteer_d("nationalparkcolors::Arches")


# -------------------------------------------------------------------------------------------------
# Q1: Do newer languages show a consistent growth trend while older languages slip into irrelevancy?
# P3: honed in average trend per era over time
# A:  avg_popularity ~ era * year
# -------------------------------------------------------------------------------------------------

# average popularity change as the years progress for each era
era_yearly <- processed_data %>%
  group_by(era, year) %>%
  summarise(avg_popularity = mean(popularity), .groups = "drop")

avg_era <- ggplot(era_yearly, aes(x = year, y = avg_popularity, color = era)) +
  geom_line(linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  labs(y = "avg popularity (%)", title = "era avg popularity") +
  scale_color_paletteer_d("nationalparkcolors::Arches")

print("PLOT 2 INFORMATION")
growth_model <- lm(formula = avg_popularity ~ era * year, data = era_yearly)
summary(growth_model)

# -------------------------------------------------------------------------------------------------
# Q2: Are there any older languages that are outperforming expectations?
# P4: each languages popularity performance as time goes on. we can cross compare them individually
# A:  examine residuals for outliers
# -------------------------------------------------------------------------------------------------

individ_data <- ggplot(processed_data, aes(x = year, y = popularity, color = era)) +
  geom_line() +
  facet_wrap(~ language, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + # this simply rotates the text on the graph
  labs(x = "language", y = "popularity (%)", title = "language individual popularity") + 
  scale_color_paletteer_d("nationalparkcolors::Arches") 

  # predict what each language should score based on era trends
  processed_data %<>%
  left_join(era_yearly %>% select(era, year, avg_popularity), by = c("era", "year")) %>%
  mutate(residual = popularity - avg_popularity)

  # average residual per language — positive means outperforming, negative means underperforming
  outlier_analysis <- processed_data %>%
    group_by(language, era) %>%
    summarise(avg_residual = mean(residual), .groups = "drop") %>%
    arrange(desc(avg_residual))

  summary(outlier_analysis$avg_residual)

  print("OUTPERFORMING — above era expectations")
  outlier_analysis %>% filter(avg_residual > 0.23)

  print("UNDERPERFORMING — below era expectations")
  outlier_analysis %>% filter(avg_residual < -3.18)

# -------------------------------------------------------------------------------------------------
# Q3: Which languages grew the most vs least over the dataset's time range?
# P5: Bar chart showing overall change
# A:  Subtract most vs least over dataset
# -------------------------------------------------------------------------------------------------

change_data <- processed_data %>% group_by(language, era) %>%
                summarise(change = last(popularity) - first(popularity), .groups = "drop") %>%
                arrange(desc(change))

print("CHANGE — most to least over dataset range")
print(change_data)

change_plot <- ggplot(change_data, aes(x = reorder(language, change), y = change, fill = era)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(x = "language", y = "popularity change (%)", title = "overall popularity shift") +
  scale_fill_paletteer_d("nationalparkcolors::Arches")


ggsave("graphs/01_overall_bar.svg", plot = overall_bar, width = 10, height = 6)
ggsave("graphs/02_overall_line.svg", plot = overall_line, width = 10, height = 6)
ggsave("graphs/03_avg_era.svg", plot = avg_era, width = 10, height = 6)
ggsave("graphs/04_individ_data.svg", plot = individ_data, width = 14, height = 10)
ggsave("graphs/05_change_plot.svg", plot = change_plot, width = 10, height = 6)