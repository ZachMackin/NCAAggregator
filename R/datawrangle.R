basketball_data <- read.csv("team_box_2022.csv")
library(dplyr)
library(tidyr)


# Split the combined columns into separate columns
basketball_data <- basketball_data %>%
  separate(field_goals_made_field_goals_attempted, into = c("field_goals_made", "field_goals_attempted"), sep = "-", convert = TRUE) %>%
  separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("three_point_made", "three_point_attempted"), sep = "-", convert = TRUE) %>%
  separate(free_throws_made_free_throws_attempted, into = c("free_throws_made", "free_throws_attempted"), sep = "-", convert = TRUE)


# Calculate scores considering the overlap between field goals and three-pointers
basketball_data <- basketball_data %>%
  mutate(score = ((field_goals_made - three_point_made) * 2) + (three_point_made * 3) + free_throws_made)

# Group by game_id and home_away to compute home and away scores
home_away_scores <- basketball_data %>%
  group_by(game_id, home_away) %>%
  summarise(team_score = sum(score), .groups = "drop") %>%
  pivot_wider(names_from = home_away, values_from = team_score, names_prefix = "score_")

# Merge back with original dataset
basketball_data <- basketball_data %>%
  left_join(home_away_scores, by = "game_id")

#Calulate Number of Possesions as (FGA−OR) + TO + (0.44×FTA)
basketball_data <- basketball_data %>%
  mutate(possessions = (field_goals_attempted - offensive_rebounds) + turnovers + (0.44 * free_throws_attempted))

basketball_data <- basketball_data %>%
  arrange(team_id, season, game_date)

# Calculate cumulative stats for each team up to (but not including) the current game
basketball_data <- basketball_data %>%
  group_by(team_id, season) %>%
  mutate(
    cum_FGM = lag(cumsum(field_goals_made), default = NA),
    cum_3FGM = lag(cumsum(three_point_made), default = NA),
    cum_FGA = lag(cumsum(field_goals_attempted), default = NA),
    cum_OR = lag(cumsum(offensive_rebounds), default = NA),
    cum_DR = lag(cumsum(defensive_rebounds), default = NA),
    cum_TO = lag(cumsum(turnovers), default = NA),
    cum_FTA = lag(cumsum(free_throws_attempted), default = NA),
    cum_possessions = lag(cumsum(possessions), default = NA),
    cum_possessions = lag(cumsum(possessions), default = NA),
    cum_points = lag(cumsum(score), default = NA)
  ) %>%
  ungroup()

# Calculate Four Factors for season-to-date stats
#eFG%  = (.5*3FGM + FGM) / FGA
#TO% = TO / Possessions
#OR% = OR / (OR + DRopp)
#FTRate = FTA / FGA
basketball_data <- basketball_data %>%
  mutate(
    eFG_pct = ifelse(!is.na(cum_FGA), ((0.5 * cum_3FGM + cum_FGM) / cum_FGA) * 100, NA),
    TO_pct = ifelse(!is.na(cum_possessions), (cum_TO / cum_possessions) * 100, NA),
    OR_pct = ifelse(!is.na(cum_OR + cum_DR), (cum_OR / (cum_OR + cum_DR)) * 100, NA),
    FT_rate = ifelse(!is.na(cum_FGA), cum_FTA / cum_FGA, NA)
  )

# Calculate opponent points and possessions by leveraging the game_id
basketball_data <- basketball_data %>%
  group_by(game_id) %>%
  mutate(
    opponent_points = score[home_away != first(home_away)],
    opponent_possessions = possessions[home_away != first(home_away)]
  ) %>%
  ungroup()

# Calculate cumulative opponent stats
basketball_data <- basketball_data %>%
  group_by(team_id, season) %>%
  mutate(
    cum_opponent_points = lag(cumsum(opponent_points), default = NA),
    cum_opponent_possessions = lag(cumsum(opponent_possessions), default = NA)
  ) %>%
  ungroup()

# Calculate offensive and defensive efficiencies
#OFF EFFICENCY team_points/team_possesions
#DEF EFFICENCY opp_points/opp_possesions
basketball_data <- basketball_data %>%
  mutate(
    offensive_efficiency = ifelse(!is.na(cum_possessions), (cum_points / cum_possessions) * 100, NA),
    defensive_efficiency = ifelse(!is.na(cum_opponent_possessions), (cum_opponent_points / cum_opponent_possessions) * 100, NA)
  )

# Calculate pace (our final metric) and filter first 5 games for each team (as these will likely not be predicative)
basketball_data <- basketball_data %>%
  arrange(team_id, season, game_date) %>% # Ensure data is ordered
  group_by(team_id, season) %>%
  mutate(
    game_number = row_number(), # Assign game numbers within the season for each team
    cum_possessions = lag(cumsum(possessions), default = NA), # Cumulative possessions
    cum_games = lag(game_number, default = NA), # Cumulative games played (lagged)
    pace = ifelse(!is.na(cum_games) & cum_games > 0, cum_possessions / cum_games, NA) # Calculate pace
  ) %>%
  filter(game_number > 5) %>% # Exclude first 5 games
  ungroup()

#Filtering out anything with NAs (two cases caused this for some games itll be one teams 6th game but another teams 5th
#also some games included D2 teams)
basketball_data <- basketball_data %>%
  filter(if_all(everything(), ~ !is.na(.)))

#Selecting just the data we need
basketball_data_wide <- basketball_data %>%
  select(
    game_id, home_away, team_id, team_name, score, eFG_pct, TO_pct, OR_pct, FT_rate,
    offensive_efficiency, defensive_efficiency, pace
  )

#Getting The data in one row per game format
regression_data <- basketball_data_wide %>%
  pivot_wider(
    id_cols = game_id,
    names_from = home_away,
    values_from = c(
      team_id, team_name, score, eFG_pct, TO_pct, OR_pct, FT_rate,
      offensive_efficiency, defensive_efficiency, pace
    ),
    names_glue = "{.value}_{tolower(home_away)}"
  )
