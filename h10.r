# 296 final project

# annoying startup messages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(scales))

# libraries i need
library(tidyverse)
library(reshape2)
library(ggplot2)
library(magrittr)
library(scales)

# C# has a pound, ensure the disabling of comments
lang_data <- read.table("languages.csv", header = TRUE, sep = ",", comment.char = "")

# for my testing, the data was in wide format. to convert it to long, we used melt and set the variable to the language, with the value being the property. that date was the id, and tells melt to leave the date alone
# we can pipe this result into a mutate too, to simply delete the month off of the data se we only have 20 entries per language
processed_data <- lang_data %>% melt(id.vars = "Date", variable.name = "language", value.name = "popularity") %>% mutate(year = as.integer(sub(".* ", "", Date)))

# we now needed to group each language and its respective year and summarize the popularity for each. (.group gets rid of warning message)
processed_data %<>% group_by(language, year) %>% summarise(popularity = mean(popularity), .groups = "drop")

head(processed_data)
