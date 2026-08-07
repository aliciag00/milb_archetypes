library(dplyr)
library(tibble)
library(purrr)

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


distance_gap <- function(cluster_distances) {
  ordered_distances <- sort(
    cluster_distances
  )
  ordered_distances[2] - ordered_distances[1]
}


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


find_similar_historical_players <- function(
    player_id,
    player_profiles,
    final_model_features,
    scaling_parameters,
    n_matches = 10
) {
  
  target_player <- player_profiles |>
    filter(
      player_league_season_id == player_id
    )
  
  if (nrow(target_player) != 1) {
    stop("player_id must match exactly one historical record.")
  }
  
  target_features <- target_player |>
    select(
      all_of(final_model_features)
    )
  
  target_scaled <- scale_new_player(
    player_features = target_features,
    scaling_parameters = scaling_parameters
  )
  
  historical_scaled <- player_profiles |>
    select(
      all_of(final_model_features)
    ) |>
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
  
  distances <- sqrt(
    rowSums(
      (
        as.matrix(
          historical_scaled[
            ,
            final_model_features
          ]
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
  
  player_profiles |>
    mutate(
      similarity_distance = distances
    ) |>
    filter(
      player_league_season_id != player_id
    ) |>
    arrange(similarity_distance) |>
    select(
      playerName,
      requested_season,
      requested_level,
      requested_league_name,
      teamName,
      cluster,
      archetype_name,
      similarity_distance
    ) |>
    slice_head(
      n = n_matches
    )
}


find_similar_new_player <- function(
    plate_appearances,
    strikeouts,
    pitches_per_plate_appearance,
    iso,
    babip,
    stolen_bases,
    caught_stealing,
    ground_outs_to_air_outs,
    player_profiles,
    final_model_features,
    scaling_parameters,
    n_matches = 10
) {
  
  new_player_features <- calculate_new_player_features(
    plate_appearances = plate_appearances,
    strikeouts = strikeouts,
    pitches_per_plate_appearance =
      pitches_per_plate_appearance,
    iso = iso,
    babip = babip,
    stolen_bases = stolen_bases,
    caught_stealing = caught_stealing,
    ground_outs_to_air_outs =
      ground_outs_to_air_outs
  )
  
  new_player_scaled <- scale_new_player(
    player_features = new_player_features,
    scaling_parameters = scaling_parameters
  )
  
  historical_scaled <- player_profiles |>
    select(
      all_of(final_model_features)
    ) |>
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
  
  distances <- sqrt(
    rowSums(
      (
        as.matrix(
          historical_scaled[
            ,
            final_model_features
          ]
        ) -
          matrix(
            new_player_scaled,
            nrow = nrow(historical_scaled),
            ncol = length(new_player_scaled),
            byrow = TRUE
          )
      )^2
    )
  )
  
  player_profiles |>
    mutate(
      similarity_distance = distances
    ) |>
    arrange(similarity_distance) |>
    select(
      playerName,
      requested_season,
      requested_level,
      requested_league_name,
      teamName,
      cluster,
      archetype_name,
      similarity_distance
    ) |>
    slice_head(
      n = n_matches
    )
}


find_similar_from_feature_row <- function(
    feature_row,
    player_profiles,
    final_model_features,
    scaling_parameters,
    n_matches = 10
) {
  
  target_scaled <- scale_new_player(
    player_features = feature_row,
    scaling_parameters = scaling_parameters
  )
  
  historical_scaled <- player_profiles |>
    select(all_of(final_model_features)) |>
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
  
  distances <- sqrt(
    rowSums(
      (
        as.matrix(
          historical_scaled[, final_model_features]
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
  
  player_profiles |>
    mutate(
      similarity_distance = distances
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
    slice_head(n = n_matches)
}