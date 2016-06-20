source("R/elo_functions.R")

results <- read.csv("data/results.csv", stringsAsFactors = FALSE)

rate_tournament(results)
