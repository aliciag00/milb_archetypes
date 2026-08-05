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

clustering_record_key <- readRDS(
  "Data/Final/Clustering_record_key.rds"
)

final_model_features <- readRDS(
  "Models/Final_model_features.rds"
)

dim(model_matrix_scaled)

set.seed(135)

k_values <- 2:10

kmeans_model <- map(k_values,function(current_k) {
  
  set.seed(135)
  
  kmeans(
    model_matrix_scaled,
    centers = current_k,
    nstart = 50,
    iter.max = 100
  )
})

# step 5
convergence_summary <- tibble(
  k = k_values,
  iterations = map_int(kmeans_model, "iter"),
  ifault = map_int(kmeans_model, "ifault")
)

convergence_summary

# 6 within-cluster sum of squares
wss_results <- tibble(k = k_values,
                      total_within_ss = map_dbl(
                        kmeans_model, "tot.withinss"
                      ))
wss_results

# 7 Elbow Plot
elbow_plot <- ggplot(
  wss_results,
  aes(
    x = k,
    y = total_within_ss
  )
) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = k_values
  ) +
  labs(
    title = "Elbow Method for K-Means Clustering",
    subtitle = "Qualified MiLB hitters, 2022–2025",
    x = "Number of clusters",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal()

elbow_plot

ggsave(
  "Outputs/Figures/Kmeans_elbow_plot.png",
  elbow_plot,
  width = 8,
  height = 5
)

player_distances <- dist(
  model_matrix_scaled,
  method = "euclidean"
)

silhouette_results <- map2_dfr(
  kmeans_model,
  k_values,
  function(model, current_k) {
    
    silhouette_object <- silhouette(
      model$cluster,
      player_distances
    )
    
    tibble(
      k = current_k,
      average_silhouette =
        mean(silhouette_object[, "sil_width"])
    )
  }
)

silhouette_results

silhouette_plot <- ggplot(
  silhouette_results,
  aes(
    x = k,
    y = average_silhouette
  )
) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = k_values
  ) +
  labs(
    title = "Average Silhouette Score by Number of Clusters",
    x = "Number of clusters",
    y = "Average silhouette score"
  ) +
  theme_minimal()

silhouette_plot

ggsave(
  "Outputs/Figures/Kmeans_silhouette_scores.png",
  silhouette_plot,
  width = 8,
  height = 5
)

silhouette_results |> 
  arrange(desc(average_silhouette))


# step 10
cluster_selection_metrics <- wss_results |>
  left_join(
    silhouette_results,
    by = "k"
  ) |>
  left_join(
    convergence_summary,
    by = "k"
  )

cluster_selection_metrics

write_csv(
  cluster_selection_metrics,
  "Outputs/Tables/Cluster_selection_metrics.csv"
)

#step 11
cluster_size_summary <- map2_dfr(
  kmeans_model,
  k_values,
  function(model, current_k) {
    
    tibble(
      cluster = model$cluster
    ) |>
      count(
        cluster,
        name = "player_records"
      ) |>
      mutate(
        k = current_k,
        cluster_percent =
          player_records /
          sum(player_records) * 100
      )
  }
)

cluster_size_summary

# smallest cluster by k
cluster_size_summary |>
  group_by(k) |>
  summarise(
    smallest_cluster = min(player_records),
    smallest_percent = min(cluster_percent),
    largest_cluster = max(player_records),
    largest_percent = max(cluster_percent),
    .groups = "drop"
  )

write_csv(
  cluster_size_summary,
  "Outputs/Tables/Cluster_size_summary.csv"
)

# step 12
candidate_k_values <- c(4, 5, 6)

candidate_models <- kmeans_model[match(candidate_k_values, k_values)]

names(candidate_models) <- paste0("k_", candidate_k_values)

# 13
get_scaled_centers <- function(model, current_k) {
  
  as_tibble(
    model$centers,
    rownames = "cluster"
  ) |>
    mutate(
      k = current_k,
      cluster = as.integer(cluster)
    ) |>
    relocate(
      k,
      cluster
    )
}

candidate_scaled_centers <- map2_dfr(
  candidate_models,
  candidate_k_values,
  get_scaled_centers
)

candidate_scaled_centers

write_csv(
  candidate_scaled_centers,
  "Outputs/Tables/Candidate_cluster_centers_scaled.csv"
)

# step 14 Interpret the standardized centers

candidate_scaled_centers |> 
  filter(k == 4)

candidate_scaled_centers |> 
  filter(k == 5)

candidate_scaled_centers |> 
  filter(k == 6)

candidate_scaled_centers |> 
  filter(k == 7)

cluster_interpretation_notes <- candidate_scaled_centers |>
  mutate(
    preliminary_label = NA_character_,
    interpretation_notes = NA_character_
  )

write_csv(
  cluster_interpretation_notes,
  "Outputs/Tables/Candidate_cluster_interpretation_notes.csv"
)

# step 15
scaling_parameters <- read_csv(
  "Models/Scaling_parameters.csv",
  show_col_types = FALSE
)

unscale_centers <- function(
    model,
    current_k,
    scaling_parameters
) {
  
  scaled_centers <- as_tibble(
    model$centers,
    rownames = "cluster"
  )
  
  unscaled_centers <- scaled_centers
  
  for (feature_name in final_model_features) {
    
    feature_center <- scaling_parameters |>
      filter(feature == feature_name) |>
      pull(center)
    
    feature_scale <- scaling_parameters |>
      filter(feature == feature_name) |>
      pull(scale)
    
    unscaled_centers[[feature_name]] <-
      scaled_centers[[feature_name]] *
      feature_scale +
      feature_center
  }
  
  unscaled_centers |>
    mutate(
      k = current_k,
      cluster = as.integer(cluster)
    ) |>
    relocate(
      k,
      cluster
    )
}

candidate_unscaled_centers <- map2_dfr(
  candidate_models,
  candidate_k_values,
  ~ unscale_centers(
    model = .x,
    current_k = .y,
    scaling_parameters = scaling_parameters
  )
)

candidate_unscaled_centers

write_csv(
  candidate_unscaled_centers,
  "Outputs/Tables/Candidate_cluster_centers_unscaled.csv"
)

# 16 attach the clusters to players

candidate_assignments <- map2_dfr(
  candidate_models,
  candidate_k_values,
  function(model, current_k) {
    
    clustering_record_key |>
      mutate(
        k = current_k,
        cluster = model$cluster
      )
  }
)

candidate_assignments

write_csv(
  candidate_assignments,
  "Outputs/Tables/Candidate_player_cluster_assignments.csv"
)

candidate_assignments |> 
  count(k)

# step 17

model_matrix_unscaled <- readRDS("Data/Final/Model_Matrix_Unscaled.rds")

selected_candidate_k <- 5

selected_assignments <- candidate_assignments |> 
  filter(k == selected_candidate_k)

nrow(selected_assignments) == nrow(model_matrix_unscaled)

# combine player info with model variables
selected_player_profiles <- bind_cols(selected_assignments, 
                                      model_matrix_unscaled)

selected_cluster_summary <- selected_player_profiles |>
  group_by(cluster) |>
  summarise(
    across(
      all_of(final_model_features),
      mean
    ),
    player_records = n(),
    .groups = "drop"
  )

selected_cluster_summary

set.seed(135)

selected_player_profiles |>
  group_by(cluster) |>
  slice_sample(n = 10) |>
  select(
    cluster,
    playerName,
    requested_season,
    requested_level,
    all_of(final_model_features)
  ) |>
  arrange(cluster)

# 18 final

model_comparison <- tibble(
  k = candidate_k_values,
  
  silhouette_score = map_dbl(
    candidate_k_values,
    ~ silhouette_results |>
      filter(k == .x) |>
      pull(average_silhouette)
  ),
  
  smallest_cluster_percent = map_dbl(
    candidate_k_values,
    ~ cluster_size_summary |>
      filter(k == .x) |>
      summarise(
        value = min(cluster_percent)
      ) |>
      pull(value)
  ),
  
  archetypes_interpretable = NA,
  clusters_too_similar = NA,
  notes = NA_character_
)

model_comparison

write_csv(
  model_comparison,
  "Outputs/Tables/Manual_cluster_model_comparison.csv"
)

candidate_scaled_centers |> 
  filter(k == 4) |> 
  arrange(cluster)
