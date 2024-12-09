library(dplyr)
library(tidyr)
#setting global variables to avoid note
globalVariables(c(
  "field_goals_made_field_goals_attempted", "three_point_field_goals_made_three_point_field_goals_attempted",
  "free_throws_made_free_throws_attempted", "field_goals_made", "three_point_made", "free_throws_made",
  "game_id", "home_away", "score", "team_score", "field_goals_attempted", "offensive_rebounds", "turnovers",
  "free_throws_attempted", "team_id", "season", "game_date", "defensive_rebounds", "possessions", "cum_FGA",
  "cum_3FGM", "cum_FGM", "cum_possessions", "cum_TO", "cum_OR", "cum_DR", "cum_FTA", "cum_points",
  "cum_opponent_possessions", "cum_opponent_points", "game_number", "cum_games", "team_name", "eFG_pct",
  "TO_pct", "OR_pct", "FT_rate", "offensive_efficiency", "defensive_efficiency", "pace", "opponent_points",
  "opponent_possessions", "turnovers"
))
#' Title
#'
#' @param basketball_data <- takes in a dataset of the form of that recieved from hoopR Data (https://github.com/sportsdataverse/hoopR-data/tree/main)
#'
#' @return Returns the dataset in a form that works for our aggregation and model functions, does so by computing a variety of metrics and cleaning up the data
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' #reading in our sample data
#' file_path <- system.file("extdata", "team_box_2022.csv", package = "NCAAggregator")
#' basketball_data <- read.csv(file_path)
#' wrangle_basketball_data(basketball_data)
wrangle_basketball_data <- function(basketball_data) {

  # Split the combined columns into separate columns
  # Calculate scores considering the overlap between field goals and three-pointers
  basketball_data <- basketball_data %>%
    separate(field_goals_made_field_goals_attempted, into = c("field_goals_made", "field_goals_attempted"), sep = "-", convert = TRUE) %>%
    separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("three_point_made", "three_point_attempted"), sep = "-", convert = TRUE) %>%
    separate(free_throws_made_free_throws_attempted, into = c("free_throws_made", "free_throws_attempted"), sep = "-", convert = TRUE) %>%
    mutate(score = ((field_goals_made - three_point_made) * 2) + (three_point_made * 3) + free_throws_made)

  # Group by game_id and home_away to compute home and away scores
  home_away_scores <- basketball_data %>%
    group_by(game_id, home_away) %>%
    summarise(team_score = sum(score), .groups = "drop") %>%
    pivot_wider(names_from = home_away, values_from = team_score, names_prefix = "score_")

  # Merge back with original dataset
  basketball_data <- basketball_data %>%
    left_join(home_away_scores, by = "game_id") %>%
    #Calulate Number of Possesions as (FGA−OR) + TO + (0.44×FTA)
    mutate(possessions = (field_goals_attempted - offensive_rebounds) + turnovers + (0.44 * free_throws_attempted)) %>%
    arrange(team_id, season, game_date) %>%
    group_by(team_id, season) %>%
    # Calculate cumulative stats for each team up to (but not including) the current game
    mutate(
      cum_FGM = lag(cumsum(field_goals_made), default = NA),
      cum_3FGM = lag(cumsum(three_point_made), default = NA),
      cum_FGA = lag(cumsum(field_goals_attempted), default = NA),
      cum_OR = lag(cumsum(offensive_rebounds), default = NA),
      cum_DR = lag(cumsum(defensive_rebounds), default = NA),
      cum_TO = lag(cumsum(turnovers), default = NA),
      cum_FTA = lag(cumsum(free_throws_attempted), default = NA),
      cum_possessions = lag(cumsum(possessions), default = NA),
      cum_points = lag(cumsum(score), default = NA)
    ) %>%
    ungroup() %>%
    # Calculate Four Factors for season-to-date stats
    #eFG%  = (.5*3FGM + FGM) / FGA
    #TO% = TO / Possessions
    #OR% = OR / (OR + DRopp)
    #FTRate = FTA / FGA
    mutate(
      eFG_pct = ifelse(!is.na(cum_FGA), ((0.5 * cum_3FGM + cum_FGM) / cum_FGA) * 100, NA),
      TO_pct = ifelse(!is.na(cum_possessions), (cum_TO / cum_possessions) * 100, NA),
      OR_pct = ifelse(!is.na(cum_OR + cum_DR), (cum_OR / (cum_OR + cum_DR)) * 100, NA),
      FT_rate = ifelse(!is.na(cum_FGA), cum_FTA / cum_FGA, NA)
    ) %>%
    group_by(game_id) %>%
    # Calculate opponent points and possessions by leveraging the game_id
    mutate(
      opponent_points = score[home_away != first(home_away)],
      opponent_possessions = possessions[home_away != first(home_away)]
    ) %>%
    ungroup() %>%
    group_by(team_id, season) %>%
    # Calculate cumulative opponent stats
    mutate(
      cum_opponent_points = lag(cumsum(opponent_points), default = NA),
      cum_opponent_possessions = lag(cumsum(opponent_possessions), default = NA)
    ) %>%
    ungroup() %>%
    # Calculate offensive and defensive efficiencies
    #OFF EFFICENCY team_points/team_possesions
    #DEF EFFICENCY opp_points/opp_possesions
    mutate(
      offensive_efficiency = ifelse(!is.na(cum_possessions), (cum_points / cum_possessions) * 100, NA),
      defensive_efficiency = ifelse(!is.na(cum_opponent_possessions), (cum_opponent_points / cum_opponent_possessions) * 100, NA)
    ) %>%
    arrange(team_id, season, game_date) %>%
    group_by(team_id, season) %>%
    # Calculate pace (our final metric) and filter first 5 games for each team (as these will likely not be predicative)
    mutate(
      game_number = row_number(),
      cum_possessions = lag(cumsum(possessions), default = NA),
      cum_games = lag(game_number, default = NA),
      pace = ifelse(!is.na(cum_games) & cum_games > 0, cum_possessions / cum_games, NA)
    ) %>%
    # Exclude first 5 games
    filter(game_number > 5)  %>%
    ungroup()

  #Selecting just the data we need
  #Getting The data in one row per game format
  regression_data <- basketball_data %>%
    select(
      game_id, home_away, team_id, team_name, score, eFG_pct, TO_pct, OR_pct, FT_rate,
      offensive_efficiency, defensive_efficiency, pace
    ) %>%
    pivot_wider(
      id_cols = game_id,
      names_from = home_away,
      values_from = c(
        team_id, team_name, score, eFG_pct, TO_pct, OR_pct, FT_rate,
        offensive_efficiency, defensive_efficiency, pace
      ),
      names_glue = "{.value}_{tolower(home_away)}"
    )

  #Filtering out anything with NAs (two cases caused this for some games itll be one teams 6th game but another teams 5th
  #also some games included D2 teams)
  regression_data <- drop_na(regression_data)
  return(regression_data)
}
