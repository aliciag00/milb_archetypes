library(dplyr)
library(readr)
library(tibble)
library(purrr)

source("Shiny/R/Model_functions.R")

exists("find_similar_historical_players")
exists("find_similar_new_player")
exists("assign_new_player_cluster")

player_cluster_profiles <- readRDS("Data/Final/Player_cluster_profiles.rds")

final_model_features <- readRDS(
  "Models/Final_model_features.rds"
)

scaling_parameters <- read_csv(
  "Models/Scaling_parameters.csv",
  show_col_types = FALSE
)

final_model_features

all(final_model_features %in%
      names(player_cluster_profiles))
target_player <- player_cluster_profiles |> 
  slice(1)

target_player |> 
  select(playerName,
         requested_season,
         teamName,
         cluster,
         archetype_name)
target_features <- target_player |> 
  select(all_of(final_model_features))

target_features

target_scaled <- scale_new_player(
  player_features = target_features,
  scaling_parameters = scaling_parameters
)

target_scaled

# step 5 - everything above runs

historical_features <- player_cluster_profiles |>
  select(
    all_of(final_model_features)
  )

historical_scaled <- historical_features |>
  mutate(
    across(
      all_of(final_model_features),
      ~ (
        .x -
          scaling_parameters$center[
            match(
              cur_column(),
              scaling_parameters$feature
            )
          ]
      ) /
        scaling_parameters$scale[
          match(
            cur_column(),
            scaling_parameters$feature
          )
        ]
    )
  )

glimpse(historical_scaled)

# add to every player
similarity_distances <- historical_scaled |>
  mutate(
    similarity_distance =
      sqrt(
        rowSums(
          (
            as.matrix(
              select(
                historical_scaled,
                all_of(final_model_features)
              )
            ) -
              matrix(
                target_scaled,
                nrow = nrow(historical_scaled),
                ncol = length(target_scaled),
                byrow = TRUE
              )
          )^2
        )
      )
  )

summary(similarity_distances$similarity_distance)

# step 7 attach player info
similar_players <- player_cluster_profiles |> 
  mutate(similarity_distance = similarity_distances$similarity_distance)

closest_players <- similar_players |>
  filter(
    player_league_season_id !=
      target_player$player_league_season_id
  ) |>
  arrange(similarity_distance) |>
  select(
    playerName,
    requested_season,
    requested_level,
    teamName,
    cluster,
    archetype_name,
    similarity_distance
  ) |>
  slice_head(n = 10)

View(closest_players)

# create function


test_player_id <- player_cluster_profiles$player_league_season_id[1]

test_similar_players <- find_similar_historical_players(
  player_id = test_player_id,
  player_profiles = player_cluster_profiles,
  final_model_features = final_model_features,
  scaling_parameters = scaling_parameters,
  n_matches = 10
)

View(test_similar_players)

# final check
all.equal(closest_players$similarity_distance,
          test_similar_players$similarity_distance,
          tolerance = 1e-10)


# test function with fake player
new_player_comps <-
  find_similar_new_player(
    plate_appearances = 450,
    strikeouts = 110,
    pitches_per_plate_appearance = 3.95,
    iso = 0.180,
    babip = 0.310,
    stolen_bases = 15,
    caught_stealing = 4,
    ground_outs_to_air_outs = 1.10,
    player_profiles = player_cluster_profiles,
    final_model_features = final_model_features,
    scaling_parameters = scaling_parameters,
    n_matches = 10
  )

View(new_player_comps)

new_player_assignment <-
  assign_new_player_cluster(
    player_name = "Test Player",
    plate_appearances = 450,
    strikeouts = 110,
    pitches_per_plate_appearance = 3.95,
    iso = 0.180,
    babip = 0.310,
    stolen_bases = 15,
    caught_stealing = 4,
    ground_outs_to_air_outs = 1.10
  )

new_player_assignment$summary

new_player_comps |> 
  count(cluster, archetype_name, sort = TRUE)

validation_player <- player_cluster_profiles |>
  slice(1)

validation_player |>
  select(
    playerName,
    requested_season,
    requested_level,
    teamName
  )

names(player_cluster_profiles)

# helper function to validate

validation_feature_row <- player_cluster_profiles |> 
  slice(1) |> 
  select(all_of(final_model_features))

validation_comps <- find_similar_from_feature_row(
  feature_row = validation_feature_row,
  player_profiles = player_cluster_profiles,
  final_model_features = final_model_features,
  scaling_parameters = scaling_parameters,
  n_matches = 10
)

View(validation_comps)
