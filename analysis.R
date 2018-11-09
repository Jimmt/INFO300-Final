library(dplyr)

# From https://www.kaggle.com/drgilermo/nba-players-stats
stats <- read.csv(file="Seasons_Stats.csv", header=TRUE, sep=",")

# From https://data.world/datadavis/nba-salaries/
salaries <- read.csv(file="nba_salaries_1990_to_2018", header=TRUE, sep=",")