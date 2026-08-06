library(dplyr)
library(readr)
library(tibble)
library(purrr)

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
calculate_new_player_features <- function(
    plate_appearances,
    strikeouts,
    pitches_per_plate_appearance,
    iso,
    babip,
    stolen_bases,
    caught_stealing,
    ground_outs_to_air_outs
) {
  
  if (
    is.na(plate_appearances) ||
    plate_appearances <= 0
  ) {
    stop("Plate appearances must be greater than zero.")
  }
  
  counting_stats <- c(
    strikeouts,
    stolen_bases,
    caught_stealing
  )
  
  if (
    any(is.na(counting_stats)) ||
    any(counting_stats < 0)
  ) {
    stop(
      paste(
        "Strikeouts, stolen bases, and caught stealing",
        "must be nonnegative numbers."
      )
    )
  }
  
  if (strikeouts > plate_appearances) {
    stop(
      "Strikeouts cannot exceed plate appearances."
    )
  }
  
  if (
    is.na(pitches_per_plate_appearance) ||
    pitches_per_plate_appearance <= 0
  ) {
    stop(
      "Pitches per plate appearance must be greater than zero."
    )
  }
  
  if (
    is.na(iso) ||
    iso < 0
  ) {
    stop("ISO must be zero or greater.")
  }
  
  if (
    is.na(babip) ||
    babip < 0 ||
    babip > 1
  ) {
    stop("BABIP must be between zero and one.")
  }
  
  if (
    is.na(ground_outs_to_air_outs) ||
    ground_outs_to_air_outs < 0
  ) {
    stop(
      paste(
        "Ground-outs-to-air-outs ratio",
        "must be zero or greater."
      )
    )
  }
  
  k_rate <-
    strikeouts / plate_appearances
  
  stolen_base_attempts <-
    stolen_bases + caught_stealing
  
  sb_attempt_rate <-
    stolen_base_attempts /
    plate_appearances
  
  sb_attempt_rate_log <-
    log1p(sb_attempt_rate)
  
  ground_air_ratio_log <-
    log1p(ground_outs_to_air_outs)
  
  feature_row <- tibble(
    pitchesPerPlateAppearance =
      pitches_per_plate_appearance,
    k_rate = k_rate,
    iso = iso,
    babip = babip,
    sb_attempt_rate_log =
      sb_attempt_rate_log,
    ground_air_ratio_log =
      ground_air_ratio_log
  )
  
  feature_row |>
    select(
      all_of(final_model_features)
    )
}

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

# 6 scaling function
scale_new_player <- function(
    player_features,
    scaling_parameters
) {
  
  missing_features <- setdiff(
    final_model_features,
    names(player_features)
  )
  
  if (length(missing_features) > 0) {
    stop(
      paste(
        "Missing required features:",
        paste(
          missing_features,
          collapse = ", "
        )
      )
    )
  }
  
  scaled_values <- map_dbl(
    final_model_features,
    function(feature_name) {
      
      raw_value <-
        player_features[[feature_name]]
      
      training_center <-
        scaling_parameters |>
        filter(
          feature == feature_name
        ) |>
        pull(center)
      
      training_scale <-
        scaling_parameters |>
        filter(
          feature == feature_name
        ) |>
        pull(scale)
      
      if (
        length(training_center) != 1 ||
        length(training_scale) != 1
      ) {
        stop(
          paste(
            "Scaling parameters are missing",
            "or duplicated for",
            feature_name
          )
        )
      }
      
      if (
        is.na(training_scale) ||
        training_scale == 0
      ) {
        stop(
          paste(
            "Invalid training scale for",
            feature_name
          )
        )
      }
      
      (
        raw_value -
          training_center
      ) /
        training_scale
    }
  )
  
  names(scaled_values) <-
    final_model_features
  
  scaled_values
}

# 7 

test_scaled <- scale_new_player(
  player_features = test_features, scaling_parameters = scaling_parameters
)

print(test_scaled)

length(test_scaled) == length(final_model_features)

# 8 
calculate_cluster_distances <- function(
    scaled_player,
    cluster_centers
) {
  
  if (
    !identical(
      names(scaled_player),
      colnames(cluster_centers)
    )
  ) {
    stop(
      paste(
        "The new-player feature order does not",
        "match the cluster-center feature order."
      )
    )
  }
  
  apply(
    cluster_centers,
    1,
    function(cluster_center) {
      
      sqrt(
        sum(
          (
            scaled_player -
              cluster_center
          )^2
        )
      )
    }
  )
}

test_distances <- calculate_cluster_distances(
  scaled_player = test_scaled,
  cluster_centers = final_kmeans_model$centers
)

print(test_distances)

which.min(test_distances)

# 9 distance gaps

distance_gap <- function(cluster_distances) {
  ordered_distances <- sort(
    cluster_distances
  )
  ordered_distances[2] - ordered_distances[1]
}

test_distance_gap <- distance_gap(test_distances)

print(test_distance_gap)

# 10 - complete function

assign_new_player_cluster <- function(
    player_name,
    plate_appearances,
    strikeouts,
    pitches_per_plate_appearance,
    iso,
    babip,
    stolen_bases,
    caught_stealing,
    ground_outs_to_air_outs
) {
  
  # 1. Calculate the model features
  player_features <-
    calculate_new_player_features(
      plate_appearances =
        plate_appearances,
      strikeouts =
        strikeouts,
      pitches_per_plate_appearance =
        pitches_per_plate_appearance,
      iso =
        iso,
      babip =
        babip,
      stolen_bases =
        stolen_bases,
      caught_stealing =
        caught_stealing,
      ground_outs_to_air_outs =
        ground_outs_to_air_outs
    )
  
  # 2. Apply the original training scaling
  scaled_player <-
    scale_new_player(
      player_features =
        player_features,
      scaling_parameters =
        scaling_parameters
    )
  
  # 3. Calculate distance to each cluster center
  cluster_distances <-
    calculate_cluster_distances(
      scaled_player =
        scaled_player,
      cluster_centers =
        final_kmeans_model$centers
    )
  
  # 4. Identify the closest cluster
  assigned_cluster <-
    which.min(cluster_distances)
  
  # 5. Find the cluster's archetype name
  assigned_label <-
    cluster_labels |>
    filter(
      cluster ==
        assigned_cluster
    ) |>
    pull(archetype_name)
  
  if (length(assigned_label) != 1) {
    stop(
      paste(
        "A unique archetype label could not",
        "be found for the assigned cluster."
      )
    )
  }
  
  # 6. Compare the two closest clusters
  ordered_distances <-
    sort(cluster_distances)
  
  assignment_distance_gap <-
    ordered_distances[2] -
    ordered_distances[1]
  
  # 7. Create a practical interpretation
  assignment_note <- case_when(
    assignment_distance_gap < 0.10 ~
      paste(
        "This player falls near the boundary",
        "between multiple archetypes."
      ),
    
    assignment_distance_gap < 0.25 ~
      paste(
        "This player has a moderately close",
        "secondary archetype."
      ),
    
    TRUE ~
      paste(
        "This player is more clearly aligned",
        "with the assigned archetype."
      )
  )
  
  # 8. Create a table of all cluster distances
  distance_table <- tibble(
    cluster =
      seq_along(
        cluster_distances
      ),
    
    distance =
      as.numeric(
        cluster_distances
      )
  ) |>
    left_join(
      cluster_labels,
      by = "cluster"
    ) |>
    arrange(distance)
  
  # 9. Create the main output
  result_summary <- tibble(
    player_name =
      player_name,
    
    assigned_cluster =
      assigned_cluster,
    
    archetype_name =
      assigned_label,
    
    distance_to_center =
      ordered_distances[1],
    
    second_closest_distance =
      ordered_distances[2],
    
    distance_gap =
      assignment_distance_gap,
    
    assignment_note =
      assignment_note
  )
  
  # 10. Return all useful outputs
  list(
    summary =
      result_summary,
    
    calculated_features =
      player_features,
    
    scaled_features =
      tibble(
        feature =
          names(scaled_player),
        
        scaled_value =
          as.numeric(
            scaled_player
          )
      ),
    
    cluster_distances =
      distance_table
  )
}

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
