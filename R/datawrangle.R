basketball_data <- read.csv("team_box_2022.csv")
library(dplyr)
library(tidyr)

# Split the combined columns into separate columns
basketball_data <- basketball_data %>%
  separate(field_goals_made_field_goals_attempted, into = c("field_goals_made", "field_goals_attempted"), sep = "-", convert = TRUE) %>%
  separate(three_point_field_goals_made_three_point_field_goals_attempted, into = c("three_point_made", "three_point_attempted"), sep = "-", convert = TRUE) %>%
  separate(free_throws_made_free_throws_attempted, into = c("free_throws_made", "free_throws_attempted"), sep = "-", convert = TRUE)

# Convert necessary columns to numeric
basketball_data <- basketball_data %>%
  mutate(across(c(field_goals_made, three_point_made, free_throws_made), as.numeric))

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

