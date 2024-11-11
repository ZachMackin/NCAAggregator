
#Aggregation Functions

#' Title
#'
#' @param prediction_matrix Takes in a matrix
#' @param trim The proportion of scores you would like to remove before taking the mean
#'
#' @return a numeric length two vector containing the home score and away score forecasts using a trimmed mean
#' @export
#'
#' @examples
trimmed_mean <- function(prediction_matrix, trim = 0.1) {
  #[ToDo] Implement our trimmed mean function
  home_scores <- predicition_matrix[ , 1]
  away_scores <- prediction_matrix[ , 2]

  home_trimmed <- mean(sort(home_scores)[(ceiling(length(home_scores) * trim) + 1):(length(home_scores) - ceiling(length(home_scores) * trim))])
  away_trimmed <- mean(sort(away_scores)[(ceiling(length(away_scores) * trim) + 1):(length(away_scores) - ceiling(length(away_scores) * trim))])

  return(c(home_trimmed, away_trimmed))
}

bayesian_averaging <- function(prediction_matrix, priors = NULL){
  #[ToDo] Implement Bayesian Averaging Function
}

exponential_smoothing <- function(prediction_matrix, alpha = 0.5){
  #[ToDo] Implement exponential smoothing function
}

geometric_mean <- function(prediction_matrix) {
  #[ToDo] Implement our geometric mean function
}
