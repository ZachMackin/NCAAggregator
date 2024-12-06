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
  # Solve for Margin using the equation: Win Prob = 0.0271 * Margin + 0.4707 (from KP)
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
}

#[ToDo] add some hyperparameters here (i.e #layers, learning rate, optimizer, etc)
simple_nueral_net <- function(data){
  #[ToDo] Implement the DL model
}
