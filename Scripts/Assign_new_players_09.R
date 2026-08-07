library(dplyr)
library(readr)
library(tibble)
library(purrr)

source("Shiny/R/Model_functions.R")

final_kmeans_model <- readRDS(
  "Models/Final_kmeans_model.rds"
)

final_model_features <- readRDS(
  "Models/Final_model_features.rds"
)

cluster_labels <- readRDS(
  "Models/Cluster_labels.rds"
)

scaling_parameters <- read_csv(
  "Models/Scaling_parameters.csv",
  show_col_types = FALSE
)

final_model_features
cluster_labels
scaling_parameters

# 3 test player
new_player_input <- tibble(player_name = "Test Player",
                           plate_appearances = 450,
                           strikeouts = 110,
                           pitches_per_plate_appearance = 3.95,
                           iso = 0.180,
                           babip = 0.310,
                           stolen_bases = 15,
                           caught_stealing = 4,
                           ground_outs_to_air_outs = 1.10)
new_player_input

# 4 Calculations

test_features <- calculate_new_player_features(plate_appearances = 450,
                                               strikeouts = 110,
                                               pitches_per_plate_appearance = 3.95,
                                               iso = 0.180,
                                               babip = 0.310,
                                               stolen_bases = 15,
                                               caught_stealing = 4,
                                               ground_outs_to_air_outs = 1.10)
test_features

names(test_features)

identical(names(test_features),final_model_features)


# 7 

test_scaled <- scale_new_player(
  player_features = test_features, scaling_parameters = scaling_parameters
)

print(test_scaled)

length(test_scaled) == length(final_model_features)

# 8 

test_distances <- calculate_cluster_distances(
  scaled_player = test_scaled,
  cluster_centers = final_kmeans_model$centers
)

print(test_distances)

which.min(test_distances)

# 9 distance gaps

test_distance_gap <- distance_gap(test_distances)

print(test_distance_gap)

# 10 - complete function

test_assignment <- assign_new_player_cluster(
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

print(test_assignment$summary)

test_assignment$scaled_features

test_assignment$summary$assigned_cluster == 
  test_assignment$cluster_distances$cluster[1]

# step 12
player_cluster_profiles <- readRDS("Data/Final/Player_cluster_profiles.rds")

model_matrix_unscaled <- readRDS(
  "Data/Final/Model_matrix_unscaled.rds"
)

profile_feature_check <- all.equal(
  as.numeric(
    player_cluster_profiles[1, final_model_features]
  ),
  as.numeric(
    model_matrix_unscaled[1, final_model_features]
  ),
  tolerance = 1e-10
)

print(profile_feature_check)

player_cluster_profiles |>
  select(all_of(final_model_features)) |>
  slice(1)

model_matrix_unscaled |>
  select(all_of(final_model_features)) |>
  slice(1)

validation_player <- player_cluster_profiles |> 
  slice(1)

validation_player  

validation_features <- validation_player |> 
  select(all_of(final_model_features))

validation_scaled <- scale_new_player(player_features = validation_features,
                                      scaling_parameters = scaling_parameters)


validation_distances <- calculate_cluster_distances(scaled_player = validation_scaled,
                                                    cluster_centers = final_kmeans_model$centers)

which.min(validation_distances)

validation_player$cluster

# 13 reproducable sample
set.seed(135)

validation_sample <- player_cluster_profiles |> 
  slice_sample(n=100)

validate_existing_assignment <- function(
    player_row
) {
  
  feature_row <- player_row |>
    select(
      all_of(final_model_features)
    )
  
  scaled_row <- scale_new_player(
    player_features = feature_row,
    scaling_parameters = scaling_parameters
  )
  
  distances <- calculate_cluster_distances(
    scaled_player = scaled_row,
    cluster_centers = final_kmeans_model$centers
  )
  
  tibble(
    original_cluster =
      player_row$cluster,
    
    reassigned_cluster =
      which.min(distances)
  )
}


validation_results <- map_dfr(
  seq_len(
    nrow(validation_sample)
  ),
  function(row_index) {
    
    validate_existing_assignment(
      validation_sample[
        row_index,
      ]
    )
  }
)


validation_summary <- validation_results |>
  summarise(
    matched_assignments =
      sum(
        original_cluster ==
          reassigned_cluster
      ),
    
    total_records =
      n(),
    
    match_percent =
      mean(
        original_cluster ==
          reassigned_cluster
      ) * 100
  )

print(validation_summary)
