combos <- paste0(rep(c("Canberra", "Adelaide", "Perth"), each = 6), 2000:2005)
data <- data.frame(variables = combos,
                   result = rnorm(18))

data$year <- gsub("[A-z]+([0-9]+)", "\\1", data$variables)
data$city <- gsub("([A-z]{4})[A-z]+[0-9]+", "\\1", data$variables)
data$cityyearflip <- gsub("([A-z]+)([0-9]+)", "year\\2_city\\1", data$variables)

library(tidyverse)
# List all files that start with "out_" in the given directory
files <- list.files("./data", "out_*")
# Extract parameters from file names
parameters <- str_extract_all(files, "((\\d\\.?)+)(?!csv)")

# Initialise an empty list to store our ggplot objects
mydata <- list()
for (i in seq_along(files)) {
  data <- read.csv(paste0("./data/", files[i]))
  data$abundance <- gsub("out_N(\\d+)_s[0-9]\\.?[0-9]?_r.+\\.csv", "\\1", files[i])
  data$supply <- gsub("out_N\\d+_s([0-9]\\.?[0-9]?)_r.+\\.csv", "\\1", files[i])
  data$rate <- gsub("out_N\\d+_s[0-9]\\.?[0-9]?_r(.+)\\.csv", "\\1", files[i])
  mydata[[i]] <- data
}

mydatgg <- do.call("rbind", mydata)

ggplot(mydatgg, aes(time, result)) +
  geom_line(aes(col = abundance)) +
  facet_grid(supply ~ rate) +
  theme_classic() +
  scale_colour_viridis_d()

parameters <- t(simplify2array(parameters))
parameters <- matrix(as.numeric(parameters),
                     ncol = ncol(parameters))
