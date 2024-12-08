source("C:/Users/zmack/Documents/STAT 600/NCAAggregator/R/datacleaningfunction.R")

#Setting up Data
basketball_data <- read.csv("team_box_2022.csv")
regression_data <- wrangle_basketball_data(basketball_data)

model_home <- lm(score_home ~
                   eFG_pct_home + eFG_pct_away +
                   TO_pct_home + TO_pct_away +
                   OR_pct_home + OR_pct_away +
                   FT_rate_home + FT_rate_away +
                   offensive_efficiency_home + defensive_efficiency_home +
                   offensive_efficiency_away + defensive_efficiency_away +
                   pace_home + pace_away,
                 data = regression_data)

model_away <- lm(score_away ~
                   eFG_pct_home + eFG_pct_away +
                   TO_pct_home + TO_pct_away +
                   OR_pct_home + OR_pct_away +
                   FT_rate_home + FT_rate_away +
                   offensive_efficiency_home + defensive_efficiency_home +
                   offensive_efficiency_away + defensive_efficiency_away +
                   pace_home + pace_away,
                 data = regression_data)

