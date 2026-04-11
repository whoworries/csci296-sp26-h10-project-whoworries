
# final project

library(reshape2)
library(ggplot2)
library(scales)

lang_data <- read.table("languages.csv", header = TRUE, sep = ";", quote = "", blank.lines.skip = TRUE)

head(lang_data)

