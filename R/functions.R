#some example models to utilize in the aggregator (need to consider the functionality to choose a newly trained model weights vs the pretrained models in the fx)

#[ToDo] add some hyperparameters here
nearest_neighbors <- function(data, ...) {
  #[ToDo] Implement our nearest_neighbors funcion
}

log5_model <- function(data, pythogorean_param=11.5){
  #[ToDo] Implement a log5 model
  #Calulate Pythagorean Scores for each team

  #use Log 5 to get win percentage

  #Backtrack from normal CDF to get score prediction
}

#[ToDo] add some hyperparameters here (i.e regularization term)
linear_reg_model <- function(data){
  #[ToDo] Implement the linear regression model
}

#[ToDo] add some hyperparameters here (i.e #layers, learning rate, optimizer, etc)
simple_nueral_net <- function(data){
  #[ToDo] Implement the DL model
}
