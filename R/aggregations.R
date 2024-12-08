
#Aggregation Functions

#' Title
#'
#' @param prediction_matrix Takes in a matrix with home scores in one column and away scores in the other
#' @param trim The proportion of scores you would like to remove before taking the mean
#'
#' @return a numeric length two vector containing the home score and away score forecasts using a trimmed mean
#' @export
#'
#' @examples
trimmed_mean <- function(prediction_matrix, trim = 0.1) {
  # Check if the input is a matrix
  if (!is.matrix(prediction_matrix)) {
    stop("The input must be a matrix.")
  }

  # Check if the matrix has exactly two columns
  if (ncol(prediction_matrix) != 2) {
    stop("The matrix must have exactly two columns: one for home scores and one for away scores.")
  }

  # Check if the matrix has numeric entries
  if (!is.numeric(prediction_matrix)) {
    stop("The matrix must contain numeric values.")
  }
  #[ToDo] Implement our trimmed mean function
  home_scores <- predicition_matrix[ , 1]
  away_scores <- prediction_matrix[ , 2]

  home_trimmed <- mean(sort(home_scores)[(ceiling(length(home_scores) * trim) + 1):(length(home_scores) - ceiling(length(home_scores) * trim))])
  away_trimmed <- mean(sort(away_scores)[(ceiling(length(away_scores) * trim) + 1):(length(away_scores) - ceiling(length(away_scores) * trim))])

  return(c(home_trimmed, away_trimmed))
}

bayesian_averaging <- function(prediction_matrix, priors) {
  # Check if the input is a matrix
  if (!is.matrix(prediction_matrix)) {
    stop("The input must be a matrix.")
  }

  # Check if the matrix has exactly two columns
  if (ncol(prediction_matrix) != 2) {
    stop("The matrix must have exactly two columns: one for home scores and one for away scores.")
  }

  # Check if the matrix has numeric entries
  if (!is.numeric(prediction_matrix)) {
    stop("The matrix must contain numeric values.")
  }
  # Validate priors
  if (length(priors) != nrow(prediction_matrix)) {
    stop("Length of priors must match the number of rows in prediction_matrix.")
  }
  if (any(priors < 0) || sum(priors) != 1) {
    stop("Priors must be non-negative and sum to 1.")
  }

  # Compute Bayesian averaging for home and away scores
  bayesian_home_score <- sum(prediction_matrix[, 1] * priors)
  bayesian_away_score <- sum(prediction_matrix[, 2] * priors)

  # Return the aggregated scores as a vector
  return(c(bayesian_home_score, bayesian_away_score))
}

exponential_smoothing <- function(prediction_matrix, alpha = 0.5) {
  # Check if the input is a matrix
  if (!is.matrix(prediction_matrix)) {
    stop("The input must be a matrix.")
  }

  # Check if the matrix has exactly two columns
  if (ncol(prediction_matrix) != 2) {
    stop("The matrix must have exactly two columns: one for home scores and one for away scores.")
  }

  # Check if the matrix has numeric entries
  if (!is.numeric(prediction_matrix)) {
    stop("The matrix must contain numeric values.")
  }
  # Start smoothing from the first prediction and iteratively apply smoothing for both scores
  home_score <- prediction_matrix[1, 1]
  away_score <- prediction_matrix[1, 2]

  for (i in 2:nrow(prediction_matrix)) {
    home_score <- alpha * prediction_matrix[i, 1] + (1 - alpha) * home_score
    away_score <- alpha * prediction_matrix[i, 2] + (1 - alpha) * away_score
  }

  return(c(home_score, away_score))
}

#' Title
#'
#' @param prediction_matrix A matrix with home scores in one column and away scores in the other
#'
#' @return a numeric length two vector containg the aggregated home and away score forcasts using a geometric mean
#' @export
#'
#' @examples
geometric_mean <- function(prediction_matrix) {
  # Check if the input is a matrix
  if (!is.matrix(prediction_matrix)) {
    stop("The input must be a matrix.")
  }

  # Check if the matrix has exactly two columns
  if (ncol(prediction_matrix) != 2) {
    stop("The matrix must have exactly two columns: one for home scores and one for away scores.")
  }

  # Check if the matrix has numeric entries
  if (!is.numeric(prediction_matrix)) {
    stop("The matrix must contain numeric values.")
  }
  return (apply(predicition_matrix, 2, function(x) exp(mean(log(x)))))
}
