source("R/elo_functions.R")

results <- openxlsx::read.xlsx("data/results.xlsx")
rate_tournament(results)
