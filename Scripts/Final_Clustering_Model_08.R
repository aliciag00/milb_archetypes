library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(ggplot2)
library(cluster)

model_matrix_scaled <- readRDS(
  "Data/Final/Model_matrix_scaled.rds"
)

model_matrix_unscaled <- readRDS(
  "Data/Final/Model_matrix_unscaled.rds"
)

clustering_record_key <- readRDS(
  "Data/Final/Clustering_record_key.rds"
)

final_model_features <- readRDS(
  "Models/Final_model_features.rds"
)

scaling_parameters <- read_csv(
  "Models/Scaling_parameters.csv",
  show_col_types = FALSE
)

nrow(model_matrix_scaled) ==
  nrow(model_matrix_unscaled)

nrow(model_matrix_scaled) ==
  nrow(clustering_record_key)

colnames(model_matrix_scaled)
final_model_features

final_k <- 4

# Final model choice:
#
# A four-cluster solution was selected because k = 4 and k = 5
# produced similar average silhouette scores of approximately 0.14.
#
# The k = 4 model also maintained balanced cluster sizes, with the
# smallest cluster containing approximately 18.8% of player records.
#
# The simpler four-cluster solution was preferred unless a fifth
# cluster could be shown to represent a clearly distinct archetype.

set.seed(135)

final_kmeans_model <- kmeans(
  model_matrix_scaled,
  centers = final_k,
  nstart = 100,
  iter.max = 100
)

final_kmeans_model$iter
final_kmeans_model$ifault

# 4
final_cluster_sizes <- tibble(
  cluster = final_kmeans_model$cluster
) |>
  count(
    cluster,
    name = "player_records"
  ) |>
  mutate(
    cluster_percent =
      player_records /
      sum(player_records) * 100
  ) |>
  arrange(cluster)

final_cluster_sizes

write_csv(
  final_cluster_sizes,
  "Outputs/Tables/Final_cluster_sizes.csv"
)

sum(final_cluster_sizes$cluster_percent)

# 5
player_distances <- dist(
  model_matrix_scaled,
  method = "euclidean"
)

final_silhouette <- silhouette(
  final_kmeans_model$cluster,
  player_distances
)

final_silhouette_summary <- tibble(
  average_silhouette =
    mean(final_silhouette[, "sil_width"]),
  
  median_silhouette =
    median(final_silhouette[, "sil_width"]),
  
  negative_records =
    sum(final_silhouette[, "sil_width"] < 0),
  
  negative_percent =
    mean(final_silhouette[, "sil_width"] < 0) * 100,
  
  near_boundary_percent =
    mean(final_silhouette[, "sil_width"] < 0.10) * 100
)

final_silhouette_summary

write_csv(
  final_silhouette_summary,
  "Outputs/Tables/Final_silhouette_summary.csv"
)

# 6
silhouette_by_cluster <- tibble(
  cluster =
    final_silhouette[, "cluster"],
  
  silhouette_width =
    final_silhouette[, "sil_width"]
) |>
  group_by(cluster) |>
  summarise(
    average_silhouette =
      mean(silhouette_width),
    
    median_silhouette =
      median(silhouette_width),
    
    negative_percent =
      mean(silhouette_width < 0) * 100,
    
    near_boundary_percent =
      mean(silhouette_width < 0.10) * 100,
    
    player_records = n(),
    
    .groups = "drop"
  )

silhouette_by_cluster

write_csv(
  silhouette_by_cluster,
  "Outputs/Tables/Final_silhouette_by_cluster.csv"
)

# step 7
final_centers_scaled <- as_tibble(
  final_kmeans_model$centers,
  rownames = "cluster"
) |>
  mutate(
    cluster = as.integer(cluster)
  ) |>
  arrange(cluster)

final_centers_scaled

write_csv(
  final_centers_scaled,
  "Outputs/Tables/Final_cluster_centers_scaled.csv"
)

# step 8
final_centers_unscaled <- final_centers_scaled

for (feature_name in final_model_features) {
  
  training_center <- scaling_parameters |>
    filter(feature == feature_name) |>
    pull(center)
  
  training_scale <- scaling_parameters |>
    filter(feature == feature_name) |>
    pull(scale)
  
  final_centers_unscaled[[feature_name]] <-
    final_centers_scaled[[feature_name]] *
    training_scale +
    training_center
}

final_centers_unscaled

if (
  "sb_attempt_rate_log" %in%
  names(final_centers_unscaled)
) {
  
  final_centers_unscaled <-
    final_centers_unscaled |>
    mutate(
      sb_attempt_rate =
        expm1(sb_attempt_rate_log)
    )
}

if (
  "ground_air_ratio_log" %in%
  names(final_centers_unscaled)
) {
  
  final_centers_unscaled <-
    final_centers_unscaled |>
    mutate(
      ground_air_ratio =
        expm1(ground_air_ratio_log)
    )
}

write_csv(
  final_centers_unscaled,
  "Outputs/Tables/Final_cluster_centers_unscaled.csv"
)

# assign players to clusters
player_cluster_assignments <- clustering_record_key |>
  mutate(
    cluster =
      final_kmeans_model$cluster,
    
    silhouette_width =
      final_silhouette[, "sil_width"]
  )

glimpse(player_cluster_assignments)
player_cluster_assignments |> 
  count()

write_csv(
  player_cluster_assignments,
  "Outputs/Tables/Final_player_cluster_assignments.csv"
)

# 10
player_cluster_profiles <- bind_cols(
  player_cluster_assignments,
  model_matrix_unscaled
)

nrow(player_cluster_profiles)

saveRDS(
  player_cluster_profiles,
  "Data/Final/Player_cluster_profiles.rds"
)

write_csv(
  player_cluster_profiles,
  "Data/Final/Player_cluster_profiles.csv"
)

# 11
calculate_center_distance <- function(
    row_number,
    cluster_number
) {
  
  player_values <-
    model_matrix_scaled[row_number, ]
  
  cluster_center <-
    final_kmeans_model$centers[
      cluster_number,
    ]
  
  sqrt(
    sum(
      (
        player_values -
          cluster_center
      )^2
    )
  )
}

player_cluster_profiles <-
  player_cluster_profiles |>
  mutate(
    row_number = row_number(),
    
    distance_to_center = map2_dbl(
      row_number,
      cluster,
      calculate_center_distance
    )
  )

representative_players <- player_cluster_profiles |>
  group_by(cluster) |>
  arrange(distance_to_center) |>
  slice_head(n = 10) |>
  ungroup() |>
  select(
    cluster,
    playerName,
    requested_season,
    requested_level,
    requested_league_name,
    teamName,
    age,
    distance_to_center,
    silhouette_width,
    all_of(final_model_features)
  )

representative_players

write_csv(
  representative_players,
  "Outputs/Tables/Representative_players_by_cluster.csv"
)

# 13 extreme profiles
# power
player_cluster_profiles |>
  group_by(cluster) |>
  slice_max(
    order_by = iso,
    n = 5
  ) |>
  select(
    cluster,
    playerName,
    requested_season,
    iso,
    pitchesPerPlateAppearance,
    k_rate
  ) |>
  arrange(cluster)

# contact
player_cluster_profiles |>
  group_by(cluster) |>
  slice_min(
    order_by = k_rate,
    n = 5
  ) |>
  select(
    cluster,
    playerName,
    requested_season,
    k_rate,
    iso,
    pitchesPerPlateAppearance
  ) |>
  arrange(cluster)

# speed
player_cluster_profiles |>
  group_by(cluster) |>
  slice_max(
    order_by = sb_attempt_rate_log,
    n = 5
  ) |>
  select(
    cluster,
    playerName,
    requested_season,
    sb_attempt_rate_log,
    iso,
    k_rate
  ) |>
  arrange(cluster)

# interpretation worksheet
cluster_interpretation <- final_centers_scaled |>
  left_join(
    final_cluster_sizes,
    by = "cluster"
  ) |>
  mutate(
    archetype_name = NA_character_,
    short_description = NA_character_,
    strengths = NA_character_,
    limitations = NA_character_
  )

cluster_interpretation

write_csv(
  cluster_interpretation,
  "Outputs/Tables/Final_cluster_interpretation.csv"
)

final_cluster_interpretation <- read.csv("Outputs/Tables/Final_cluster_interpretation.csv")

View(final_cluster_interpretation)

# 15
cluster_labels <- tibble(cluster = 1:4,
                         archetype_name = c("Strikeout-Prone Power Hitters",
                                            "Swinging Early Contact Hitters",
                                            "Power Hitters with Moderate Speed",
                                            "Fast, Aggressive Contact Hitters "))

player_cluster_profiles <-
  player_cluster_profiles |>
  left_join(
    cluster_labels,
    by = "cluster"
  )

# 16

saveRDS(
  final_kmeans_model,
  "Models/Final_kmeans_model.rds"
)

saveRDS(
  cluster_labels,
  "Models/Cluster_labels.rds"
)

write_csv(
  cluster_labels,
  "Models/Cluster_labels.csv"
)

saveRDS(
  final_centers_scaled,
  "Models/Final_cluster_centers_scaled.rds"
)

saveRDS(
  final_centers_unscaled,
  "Models/Final_cluster_centers_unscaled.rds"
)

saveRDS(
  player_cluster_profiles,
  "Data/Final/Player_cluster_profiles.rds"
)

write_csv(
  player_cluster_profiles,
  "Data/Final/Player_cluster_profiles.csv"
)

file.exists(
  "Models/Final_kmeans_model.rds"
)

file.exists(
  "Models/Final_cluster_centers_scaled.rds"
)

file.exists(
  "Models/Cluster_labels.rds"
)

file.exists(
  "Data/Final/Player_cluster_profiles.rds"
)
