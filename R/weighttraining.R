#This File is just to show how the Linear and NN functions

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


# Create the target variable: 1 if home team wins, 0 otherwise
regression_data <- regression_data %>%
  mutate(home_win = ifelse(score_home > score_away, 1, 0))

# Train logistic regression
logistic_model <- glm(home_win ~ eFG_pct_home + eFG_pct_away + TO_pct_home + TO_pct_away +
                        OR_pct_home + OR_pct_away + FT_rate_home + FT_rate_away +
                        offensive_efficiency_home + defensive_efficiency_home +
                        offensive_efficiency_away + defensive_efficiency_away +
                        pace_home + pace_away,
                      family = binomial, data = regression_data)


