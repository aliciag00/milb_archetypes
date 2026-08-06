library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)

modeling_data <- readRDS(
  "Data/Final/Modeling_data_provisional.rds"
)

feature_review <- read_csv(
  "Outputs/Tables/Model_feature_review.csv",
  show_col_types = FALSE
)

strong_correlations <- read_csv(
  "Outputs/Tables/Model_feature_correlations.csv",
  show_col_types = FALSE
)

# step 2- recall provisional features
provisional_features <- c(
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

provisional_features <- intersect(
  provisional_features,
  names(modeling_data)
)

provisional_features

# step 3
hit <- read_csv("Data/Cleaned/Milb_hitters_clean.csv")
View(hit)

final_features <- c(
  "pitchesPerPlateAppearance",
  "k_rate",
  "iso",
  "babip",
  "sb_attempt_rate",
  "groundOutsToAirouts"
)

final_features <- intersect(
  final_features,
  names(modeling_data)
)

final_features

# Decision Documentation
# pitchesPerPlateAppearance:
# Represents plate patience and being a tough out.
#
# k_rate:
# Represents contact ability and strikeout tendency.
#
# iso:
# Represents extra-base power independent of batting average.
#
# babip:
# Represents outcomes on balls put into play.
#
# sb_attempt_rate:
# Represents baserunning aggressiveness rather than raw stolen-base totals.
#
# groundOutsToAirouts:
# Represents ground-ball versus air-ball tendency.
#
# Excluded overlapping features:
# hr_rate and xbh_rate because there would be multicollinearity with iso
# swing and miss rate overlaps with K rate

final_feature_missingness <- tibble(
  feature = final_features,
  missing_count = map_int(
    modeling_data[final_features],
    ~ sum(is.na(.x))
  ),
  missing_percent = map_dbl(
    modeling_data[final_features],
    ~ mean(is.na(.x)) * 100
  )
)

final_feature_missingness

# step 8
modeling_data |>
  summarise(
    missing_ground_air =
      sum(is.na(groundOutsToAirouts)),
    missing_percent =
      mean(is.na(groundOutsToAirouts)) * 100,
    minimum =
      min(groundOutsToAirouts, na.rm = TRUE),
    maximum =
      max(groundOutsToAirouts, na.rm = TRUE)
  )

# step 9 - skew transformations
clustering_data <- modeling_data |>
  filter(
    if_all(
      all_of(final_features),
      ~ !is.na(.x)
    )
  )

clustering_data <- clustering_data |>
  mutate(
    sb_attempt_rate_log =
      log1p(sb_attempt_rate)
  )

if ("groundOutsToAirouts" %in% names(clustering_data)) {
  
  clustering_data <- clustering_data |>
    mutate(
      ground_air_ratio_log =
        log1p(groundOutsToAirouts)
    )
}

# step 10
library(e1071)

tibble(
  version = c(
    "Original",
    "Log transformed"
  ),
  skewness = c(
    e1071::skewness(
      clustering_data$sb_attempt_rate,
      type = 2
    ),
    e1071::skewness(
      clustering_data$sb_attempt_rate_log,
      type = 2
    )
  )
)

final_model_features <- c(
  "pitchesPerPlateAppearance",
  "k_rate",
  "iso",
  "babip",
  "sb_attempt_rate_log",
  "ground_air_ratio_log"
)

model_matrix_unscaled <- clustering_data |> 
  select(all_of(final_model_features))

glimpse(model_matrix_unscaled)
summary(model_matrix_unscaled)

map_lgl(model_matrix_unscaled, is.numeric)

# step 12
model_matrix_scaled <- scale(model_matrix_unscaled)

scaling_parameters <- tibble(
  feature = colnames(model_matrix_scaled),
  center = as.numeric(
    attr(model_matrix_scaled, "scaled:center")
  ),
  scale = as.numeric(
    attr(model_matrix_scaled, "scaled:scale")
  )
)

scaling_parameters

write_csv(
  scaling_parameters,
  "Models/Scaling_parameters.csv"
)

saveRDS(
  model_matrix_unscaled,
  "Data/Final/Model_matrix_unscaled.rds"
)

saveRDS(
  model_matrix_scaled,
  "Data/Final/Model_matrix_scaled.rds"
)

saveRDS(
  final_model_features,
  "Models/Final_model_features.rds"
)

class(model_matrix_scaled)
dim(model_matrix_scaled)

# step 14 verify scaling
round(colMeans(model_matrix_scaled),6)

round(apply(model_matrix_scaled,2,sd),6)

dir.create("Models", recursive = TRUE, showWarnings = FALSE)
write_csv(scaling_parameters,"Models/Scaling_parameters.csv")

# 16 stable row identifier
clustering_record_key <- clustering_data |> 
  select(player_league_season_id, playerId, playerName,
         requested_season, requested_level, requested_league_name,
         teamName, age)

nrow(clustering_record_key) == nrow(model_matrix_scaled)

# step 17 save final data
saveRDS(clustering_data,"Data/Final/Clustering_data.rds")

saveRDS(model_matrix_unscaled,"Data/Final/Model_matrix_unscaled.rds")

saveRDS(model_matrix_scaled,"Data/Final/Model_matrix_scaled.rds")

saveRDS(clustering_record_key,"Data/Final/Clustering_record_key.rds")

saveRDS(final_model_features, "Models/Final_model_features.rds")

# 18 preprocessing summary
preprocessing_summary <- tibble(
  metric = c(
    "Original modeling records",
    "Final clustering records",
    "Records removed for missing data",
    "Features in final model"
  ),
  value = c(
    nrow(modeling_data),
    nrow(clustering_data),
    nrow(modeling_data) - nrow(clustering_data),
    length(final_model_features)
  )
)

preprocessing_summary

write_csv(
  preprocessing_summary,
  "Outputs/Tables/Clustering_preprocessing_summary.csv"
)
