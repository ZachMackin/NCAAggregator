source("C:/Users/zmack/Documents/STAT 600/NCAAggregator/R/datacleaningfunction.R")

basketball_data <- read.csv("team_box_2022.csv")
regression_data <- wrangle_basketball_data(basketball_data)
