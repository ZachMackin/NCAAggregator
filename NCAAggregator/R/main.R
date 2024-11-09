
# Our primary function
aggregate_predictions <- function(models_or_predictions, aggregation_method, data = NULL, ...) {
  # Initialize an empty list to store predictions as vectors (each with home and away scores)
  predictions <- list()

  # 1. Loop through each element in models_or_predictions
  for (item in models_or_predictions) {
    if (is.function(item)) {
      #[ToDo] find a way to ensure the function takes in the right arguments and outputs the right prediction?
      # If the item is a function (i.e., a model), use it to generate a prediction
      predictions[[length(predictions) + 1]] <- item(data)
    } else if (is.numeric(item) && length(item) == 2) {
      # If the item is a numeric vector of length 2, treat it as a pre-generated prediction
      predictions[[length(predictions) + 1]] <- item
    } else {
      warning("Invalid item in models_or_predictions: Must be a function or a length-2 numeric vector.")
    }
  }

  # [ToDo] Convert the list of predictions to a matrix for easier aggregation


  # 2. Aggregate predictions based on specified method
  aggregated_prediction <- switch(
    aggregation_method,
    "Trimmed Mean" = trimmed_mean(prediction_matrix, ...),
    "Bayesian Averaging" = bayesian_averaging(prediction_matrix, ...),
    "Exponential Smoothing" = exponential_smoothing(prediction_matrix, ...),
    stop("Invalid aggregation method specified.")
  )

  # Return the aggregated prediction as a vector of home and away scores
  return(aggregated_prediction)
}
