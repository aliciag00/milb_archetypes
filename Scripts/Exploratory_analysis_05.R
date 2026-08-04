install.packages("e1071")

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(purrr)
library(tibble)
library(e1071)

modeling_data <- readRDS("Data/Final/Modeling_data_provisional.rds")

glimpse(modeling_data)

# step 1
model_features <- c(
  "bb_rate",
  "k_rate",
  "iso",
  "babip",
  "pitchesPerPlateAppearance",
  "swing_miss_rate",
  "hr_rate",
  "xbh_rate",
  "sb_attempt_rate",
  "groundOutsToAirouts"
)

model_features <- intersect(model_features,
                            names(modeling_data))
model_features

# step 2
modeling_data |> 
  count(complete_model_features)

modeling_data |> 
  summarise(total_rows = n(),
            complete_rows = sum(complete_model_features),
            complete_percent = mean(complete_model_features)* 100)

modeling_complete <- modeling_data |> 
  filter(complete_model_features)

# step 3

feature_summary <- modeling_complete |>
  summarise(
    across(
      all_of(model_features),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("feature", ".value"),
    names_pattern = "(.+)_(mean|sd|median|min|max)$"
  )

feature_summary

write_csv(feature_summary, "Outputs/Tables/Model_feature_summary.csv")

# step 4

features_long <- modeling_complete |>
  select(
    player_league_season_id,
    playerName,
    requested_season,
    requested_level,
    all_of(model_features)
  ) |>
  pivot_longer(
    cols = all_of(model_features),
    names_to = "feature",
    values_to = "value"
  )

glimpse(features_long)

# step 5
dir.create("Outputs/Figures",
           recursive = TRUE,
           showWarnings = FALSE)

walk(
  model_features,
  function(current_feature) {
    
    plot_data <- modeling_complete |>
      select(all_of(current_feature))
    
    p <- ggplot(
      plot_data,
      aes(x = .data[[current_feature]])
    ) +
      geom_histogram(
        bins = 30
      ) +
      labs(
        title = paste("Distribution of", current_feature),
        x = current_feature,
        y = "Player records"
      ) +
      theme_minimal()
    
    ggsave(
      filename = paste0(
        "Outputs/Figures/histogram_",
        current_feature,
        ".png"
      ),
      plot = p,
      width = 7,
      height = 5
    )
  }
)

#step 6
feature_skewness <- tibble(feature = model_features,
                           skewness = map_dbl(
                             modeling_complete[model_features],
                             ~ e1071::skewness(
                               .x, na.rm = TRUE, type = 2
                             )
                           )) |> 
  arrange(desc(abs(skewness)))

feature_skewness

write_csv(feature_skewness, "Outputs/Tables/Model_feature_skewness.csv")

walk(
  model_features,
  function(current_feature) {
    
    p <- modeling_complete |>
      ggplot(
        aes(
          y = .data[[current_feature]]
        )
      ) +
      geom_boxplot() +
      labs(
        title = paste("Boxplot of", current_feature),
        y = current_feature,
        x = NULL
      ) +
      theme_minimal()
    
    ggsave(
      filename = paste0(
        "Outputs/Figures/boxplot_",
        current_feature,
        ".png"
      ),
      plot = p,
      width = 5,
      height = 6
    )
  }
)

# step 8: outliers
outlier_summary <- map_dfr(
  model_features,
  function(current_feature) {
    
    values <- modeling_complete[[current_feature]]
    
    q1 <- quantile(values, 0.25, na.rm = TRUE)
    q3 <- quantile(values, 0.75, na.rm = TRUE)
    iqr_value <- IQR(values, na.rm = TRUE)
    
    lower_bound <- q1 - 1.5 * iqr_value
    upper_bound <- q3 + 1.5 * iqr_value
    
    tibble(
      feature = current_feature,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      outlier_count = sum(
        values < lower_bound |
          values > upper_bound,
        na.rm = TRUE
      ),
      outlier_percent = mean(
        values < lower_bound |
          values > upper_bound,
        na.rm = TRUE
      ) * 100
    )
  }
)

outlier_summary

write_csv(outlier_summary, "Outputs/Tables/Model_feature_outliers.csv")

# step 9: find the most extreme player records
extreme_players <- map_dfr(
  model_features, function(current_feature) {
    low_players <- modeling_complete |>
      arrange(.data[[current_feature]]) |>
      slice_head(n = 5) |>
      transmute(
        feature = current_feature,
        extreme_type = "Lowest",
        playerName,
        requested_season,
        requested_level,
        value = .data[[current_feature]]
      )
    
    high_players <- modeling_complete |>
      arrange(desc(.data[[current_feature]])) |>
      slice_head(n = 5) |>
      transmute(
        feature = current_feature,
        extreme_type = "Highest",
        playerName,
        requested_season,
        requested_level,
        value = .data[[current_feature]]
      )
    
    bind_rows(low_players, high_players)
  }
)

extreme_players

write_csv(extreme_players, "Outputs/Tables/Extreme_player_feature_values.csv")

# step 10 - correlation matrix
correlation_matrix <- modeling_complete |> 
  select(all_of(model_features)) |> 
  cor(use = "pairwise.complete.obs",
      method = "pearson")

round(correlation_matrix,2)

correlation_table <- as.data.frame(correlation_matrix) |> 
  rownames_to_column("feature")

write_csv(correlation_table,
          "Outputs/Tables/Model_feature_correlations.csv")

# step 11 - correlation heatmap
correlation_long <- as.data.frame(correlation_matrix) |> 
  rownames_to_column("feature_1") |> 
  pivot_longer(cols = -feature_1,
               names_to = "feature_2",
               values_to = "correlation")

correlation_plot <- ggplot(correlation_long, aes(x = feature_1,
                                                 y = feature_2, fill = correlation)) + 
  geom_tile() +
  geom_text(aes(label = round(correlation,2)), size = 3) +
  labs(title = "Correlation Matrix of Candidate Clustering Features",
       x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Outputs/Figures/Model_feature_correlation_matrix.png", correlation_plot,
       width = 10, height = 8)

# step 12
strong_correlations <- correlation_long |>
  filter(feature_1 != feature_2) |>
  mutate(
    pair = map2_chr(
      feature_1,
      feature_2,
      ~ paste(sort(c(.x, .y)), collapse = " | ")
    )
  ) |>
  distinct(pair, .keep_all = TRUE) |>
  filter(abs(correlation) >= 0.70) |>
  arrange(desc(abs(correlation)))

strong_correlations <- correlation_long |>
  filter(feature_1 != feature_2) |>
  mutate(
    pair = map2_chr(
      feature_1,
      feature_2,
      ~ paste(sort(c(.x, .y)), collapse = " | ")
    )
  ) |>
  distinct(pair, .keep_all = TRUE) |>
  filter(abs(correlation) >= 0.70) |>
  arrange(desc(abs(correlation)))

strong_correlations

# step 13 features by level
level_feature_summary <- modeling_complete |> 
  group_by(requested_level) |> 
  summarise(across(all_of(model_features),
                   ~ mean(.x, na.rm = TRUE)),
            player_records = n(),
            .groups = "drop")
level_feature_summary

write_csv(level_feature_summary, "Outputs/Tables/Feature_means_by_level.csv")

# Features by Season
season_feature_summary <- modeling_complete |>
  group_by(requested_season) |>
  summarise(
    across(
      all_of(model_features),
      ~ mean(.x, na.rm = TRUE)
    ),
    player_records = n(),
    .groups = "drop"
  )

season_feature_summary

write_csv(
  season_feature_summary,
  "Outputs/Tables/Feature_means_by_season.csv"
)

feature_review <- feature_summary |>
  left_join(
    feature_skewness,
    by = "feature"
  ) |>
  left_join(
    outlier_summary,
    by = "feature"
  ) |>
  left_join(
    feature_availability |>
      select(
        feature,
        missing_count,
        missing_percent,
        unique_values
      ),
    by = "feature"
  )

feature_review

write_csv(
  feature_review,
  "Outputs/Tables/Model_feature_review.csv"
)

# Review Notes
# 1. Which features are strongly skewed?
# - stolen base attempt, ground out to air outs, 
# 2. Which features have the most outliers?
# - 
# 3. Which features are strongly correlated?
# - xbh and iso, hr_rate and iso, pitches per plate appearance and bb_rate
# 4. Are there noticeable differences between levels?
# - not huge differences
# 5. Are there noticeable differences between seasons?
# - normal differences
# 6. Which features currently appear redundant?
# - iso and xbh, bb rate and pitches per plate appearance
# 7. Which features should remain in the first clustering model?
# - 