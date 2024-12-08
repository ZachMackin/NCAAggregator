
# Our primary function
#' Title
#'
#' @param models_or_predictions <- a vector which have elements of one of two forms, either a numeric length two vector containing the home score and away score of a prediction, or a function that takes in data (of the format explained in data) and returns a numeric length two vector containing the home score and away score
#' @param aggregation_method <- A string representing the name of the aggregation method
#' @param data <- A one row data frame with the selcted statistics (KP adj efficency, KP adj tempo, four factors, for each team)
#' @param ... <- additional arguments to be passed in our aggregator
#'
#' @return A numeric length two vector containing the home score and away score
#' @export
#'
#' @examples
aggregate_predictions <- function(models_or_predictions, aggregation_method, data = NULL, ...) {
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

  # [ToDo] Convert the list of predictions to a matrix for easier aggregation
  predicition_matrix <- do.call(rbind, predictions)

  # 2. Aggregate predictions based on specified method
  aggregated_prediction <- switch(
    aggregation_method,
    "Trimmed Mean" = trimmed_mean(prediction_matrix, ...),
    "Bayesian Averaging" = bayesian_averaging(prediction_matrix, ...),
    "Geometric Mean" = geometric_mean(predicition_matrix),
    "Exponential Smoothing" = exponential_smoothing(prediction_matrix, ...),
    "Mean" = colMeans(predicition_matrix),
    "Median" = apply(predicition_matrix, 2, median),
    stop("Invalid aggregation method specified.")
  )

  # Return the aggregated prediction as a vector of home and away scores
  return(aggregated_prediction)
}
