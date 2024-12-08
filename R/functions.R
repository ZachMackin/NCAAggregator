#some example models to utilize in the aggregator (need to consider the functionality to choose a newly trained model weights vs the pretrained models in the fx)

#[ToDo] add some hyperparameters here
efficency_model <- function(data) {
  # Adjust Offensive and Defensive Efficiencies by 1.4% (home court advantage)
  home_off_eff <- data$offensive_efficiency_home * 1.014
  home_def_eff <- data$defensive_efficiency_home * 0.986
  away_off_eff <- data$defensive_efficiency_away * 0.986
  away_def_eff <- data$offensive_efficiency_away * 1.014

  # Calculate Adjusted Efficiency Margins for Home and Away Teams
  adj_em_home <- home_off_eff - home_def_eff
  adj_em_away <- away_off_eff - away_def_eff

  #Pace calculations
  total_pace <- (data$pace_home + data$pace_away) / 2

  #Calculate Margin Using the Formula
  margin <- (adj_em_home - adj_em_away) * total_pace / 200

  #Calculate Average Score
  # Average score is the mean of Offensive Efficiency * (Pace / 100) and Defensive Efficiency * (Pace / 100)
  pace_home <- data$pace_home
  pace_away <- data$pace_away
  home_avg_score <- (home_off_eff * (pace_home / 100) + away_def_eff * (pace_away / 100)) / 2
  away_avg_score <- (away_off_eff * (pace_away / 100) + home_def_eff * (pace_home / 100)) / 2
  avg_score <- (home_avg_score + away_avg_score) / 2

  # Calculate Home and Away Scores Using the Margin
  home_score <- avg_score + (margin / 2)
  away_score <- avg_score - (margin / 2)

  return(list(home_score = home_score, away_score = away_score))

}

log5_model <- function(data, pythagorean_param=11.5){
  # Adjust Offensive and Defensive Efficiencies by 1.4% (home court advantage)
  home_off_eff <- data$offensive_efficiency_home * 1.014
  home_def_eff <- data$defensive_efficiency_home * 0.986
  away_off_eff <- data$defensive_efficiency_away * 0.986
  away_def_eff <- data$offensive_efficiency_away * 1.014

  #Calculate Pythagorean Scores for Home and Away Teams
  pythagorean_home <- (home_off_eff^pythagorean_param) /
    ((home_off_eff^pythagorean_param) + (home_def_eff^pythagorean_param))

  pythagorean_away <- (away_off_eff^pythagorean_param) /
    ((away_off_eff^pythagorean_param) + (away_def_eff^pythagorean_param))

  #Calculate Log5 Probability for Home Team
  win_prob_home <- (pythagorean_home - (pythagorean_home * pythagorean_away)) /
    (pythagorean_home + pythagorean_away - (2 * pythagorean_home * pythagorean_away))

  # Convert Win Probability to Margin
  # Solve for Margin using the equation: Win Prob = 0.0271 * Margin + 0.4707 (from KenPom Normal CDF)
  margin <- (win_prob_home - 0.4707) / 0.0271

  #Calculate Average Score
  # Average score is the mean of Offensive Efficiency * (Pace / 100) and Defensive Efficiency * (Pace / 100)
  pace_home <- data$pace_home
  pace_away <- data$pace_away
  home_avg_score <- (home_off_eff * (pace_home / 100) + away_def_eff * (pace_away / 100)) / 2
  away_avg_score <- (away_off_eff * (pace_away / 100) + home_def_eff * (pace_home / 100)) / 2
  avg_score <- (home_avg_score + away_avg_score) / 2

  # Calculate Home and Away Scores Using the Margin
  home_score <- avg_score + (margin / 2)
  away_score <- avg_score - (margin / 2)

  return(list(home_score = home_score, away_score = away_score))
}

#[ToDo] add some hyperparameters here (i.e regularization term)
linear_reg_model <- function(data){
  #[ToDo] Implement the linear regression model
  #Our Coefficients from training a linear model on the 2022 college basketball games
  coefficients_home <- c(
    intercept = -54.24337503,
    eFG_pct_home = -0.15161308,
    eFG_pct_away = 0.14775478,
    TO_pct_home = -0.22735459,
    TO_pct_away = -0.28364625,
    OR_pct_home = -0.05209067,
    OR_pct_away = -0.06627774,
    FT_rate_home = -0.16963608,
    FT_rate_away = -4.77326491,
    offensive_efficiency_home = 0.45386923,
    defensive_efficiency_home = 0.03767082,
    offensive_efficiency_away = -0.39861865,
    defensive_efficiency_away = 0.22665027,
    pace_home = 0.64974889,
    pace_away = 0.87516834
  )
  coefficients_away <- c(
    intercept = -69.7678001,
    eFG_pct_home = -0.1213718,
    eFG_pct_away = -0.3408643,
    TO_pct_home = -0.1035799,
    TO_pct_away = 0.1830721,
    OR_pct_home = -0.1296334,
    OR_pct_away = -0.1655520,
    FT_rate_home = -11.4963686,
    FT_rate_away = -4.1895024,
    offensive_efficiency_home = -0.2352713,
    defensive_efficiency_home = 0.1984820,
    offensive_efficiency_away = 0.6137461,
    defensive_efficiency_away = 0.0262994,
    pace_home = 1.0192786,
    pace_away = 0.6419960
  )
  features_name <- names(coefficients_home)[-1] # Exclude the intercept
  features_vec <- data[features_name]

  # Calculate the prediction for the home score
  home_score <- coefficients_home["intercept"] + sum(coefficients_home[features_name] * features_vec)

  # Calculate the prediction for the away score
  away_score <- coefficients_away["intercept"] + sum(coefficients_away[features_name] * features_vec)

  return (list(home_score = home_score, away_score = away_score))
}


logistic_model <- function(data){
  # Coefficients from training a logistic regression model
  coefficients <- c(
    intercept = 4.067095,
    eFG_pct_home = -0.016828,
    eFG_pct_away = 0.075402,
    TO_pct_home = -0.023484,
    TO_pct_away = -0.081895,
    OR_pct_home = 0.001401,
    OR_pct_away = 0.031390,
    FT_rate_home = 0.940597,
    FT_rate_away = -1.280705,
    offensive_efficiency_home = 0.095749,
    defensive_efficiency_home = -0.034996,
    offensive_efficiency_away = -0.150079,
    defensive_efficiency_away = 0.029773,
    pace_home = -0.036334,
    pace_away = 0.034040
  )

  features_name <- names(coefficients)[-1] # Exclude the intercept
  features_vec <- data[features_name]

  # Calculate the linear predictor
  linear_predictor <- coefficients["intercept"] + sum(coefficients[features_name] * features_vec)

  # Calculate the win probability using the logistic function
  win_probability <- 1 / (1 + exp(-linear_predictor))

  # Convert win probability to margin
  margin <- (win_probability - 0.4707) / 0.0271

  # Calculate the average score for the game
  home_off_eff <- data$offensive_efficiency_home * 1.014
  home_def_eff <- data$defensive_efficiency_home * 0.986
  away_off_eff <- data$defensive_efficiency_away * 0.986
  away_def_eff <- data$offensive_efficiency_away * 1.014
  pace_home <- data$pace_home
  pace_away <- data$pace_away
  home_avg_score <- (home_off_eff * (pace_home / 100) + away_def_eff * (pace_away / 100)) / 2
  away_avg_score <- (away_off_eff * (pace_away / 100) + home_def_eff * (pace_home / 100)) / 2
  avg_score <- (home_avg_score + away_avg_score) / 2

  # Calculate Home and Away Scores Using the Margin
  home_score <- avg_score + (margin / 2)
  away_score <- avg_score - (margin / 2)

  return(list(home_score = home_score, away_score = away_score))
}

