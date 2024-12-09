# Our primary function
#' Title
#'
#' @param models_or_predictions <- a vector which have elements of one of two forms, either a numeric length two vector containing the home score and away score of a prediction, or a function that takes in data (of the format explained in data) and returns a numeric length two vector containing the home score and away score
#' @param aggregation_method <- A string representing the name of the aggregation method (defaults to median)
#' @param data <- A list with the selcted statistics (the four factors for home and away, off and def efficency for home and away, and Pace/Tempo for home and away)
#' @param ... <- additional arguments to be passed in our aggregator
#'
#' @return A numeric length two vector containing the home score and away score
#' @export
#'
#' @examples
#' #Game Data from TAMU 11/30/2024
#' game_data <- list(
#' eFG_pct_home = 47.2,
#' eFG_pct_away = 50.8,
#' TO_pct_home = 18.3,
#' TO_pct_away = 14.6,
#' OR_pct_home = 43.6,
#' OR_pct_away = 30.2,
#' FT_rate_home = .438,
#' FT_rate_away = .397,
#' offensive_efficiency_home = 114.3,
#' defensive_efficiency_home = 94.0,
#' offensive_efficiency_away = 111.9,
#' defensive_efficiency_away = 101.4,
#' pace_home = 67.0,
#' pace_away = 70.3
#' )
#' #Input model, three score predictions (VEGAS, KENPOM, and EvanMiya), and then our four functions
#' input = c(c(78, 72.5), c(76, 70), c(79, 74),
#' efficiency_model, log5_model, linear_reg_model, logistic_model)
#' aggregate_predictions(input, aggregation_method="Geometric Mean", data=game_data)
#' #Actual Score was 81-77
#'
aggregate_predictions <- function(models_or_predictions, aggregation_method="Median", data = NULL, ...) {
  # Initialize an empty list to store predictions as vectors (each with home and away scores)
  predictions <- list()

  # 1. Loop through each element in models_or_predictions
  for (item in models_or_predictions) {
    if (is.function(item)) {
      pred <- item(data)
      # If the item is a function (i.e., a model), use it to generate a prediction
      if (is.numeric(pred) && length(pred) == 2){
        predictions[[length(predictions) + 1]] <- pred
      }
    } else if (is.numeric(item) && length(item) == 2) {
      # If the item is a numeric vector of length 2, treat it as a pre-generated prediction
      predictions[[length(predictions) + 1]] <- item
    } else {
      warning("Invalid item in models_or_predictions: Must be a function or a length-2 numeric vector.")
    }
  }

  prediction_matrix <- do.call(rbind, predictions)

  # 2. Aggregate predictions based on specified method
  aggregated_prediction <- switch(
    aggregation_method,
    "Trimmed Mean" = trimmed_mean(prediction_matrix, ...),
    "Bayesian Averaging" = bayesian_averaging(prediction_matrix, ...),
    "Geometric Mean" = geometric_mean(prediction_matrix),
    "Exponential Smoothing" = exponential_smoothing(prediction_matrix, ...),
    "Mean" = colMeans(prediction_matrix),
    "Median" = apply(prediction_matrix, 2, median),
    stop("Invalid aggregation method specified.")
  )

  # Return the aggregated prediction as a vector of home and away scores
  return(aggregated_prediction)
}
