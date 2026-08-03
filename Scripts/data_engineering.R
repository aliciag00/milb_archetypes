library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)

hitters_clean <- readRDS("Data/Cleaned/Milb_hitters_clean.rds")

glimpse(hitters_clean)

# separate id fields from model features
id_columns <- c(
  "player_league_season_id",
  "playerId",
  "playerName",
  "playerFullName",
  "requested_season",
  "requested_level",
  "requested_league_id",
  "requested_league_name",
  "teamId",
  "teamName",
  "teamAbbrev",
  "position",
  "positionAbbrev",
  "primaryPositionAbbrev",
  "age",
  "plateAppearances"
)

id_columns <- intersect(id_columns, names(hitters_clean))

candidate_names <- c(
  "plateAppearances",
  "atBats",
  "hits",
  "doubles",
  "triples",
  "homeRuns",
  "extraBaseHits",
  "walks",
  "strikeouts",
  "stolenBases",
  "caughtStealing",
  "hitByPitch",
  "totalBases",
  "totalSwings",
  "swingAndMisses",
  "ballsInPlay",
  "groundOuts",
  "flyOuts",
  "lineOuts",
  "popOuts",
  "avg",
  "obp",
  "slg",
  "ops",
  "iso",
  "babip",
  "walksPerPlateAppearance",
  "strikeoutsPerPlateAppearance",
  "homeRunsPerPlateAppearance",
  "walksPerStrikeout",
  "pitchesPerPlateAppearance",
  "stolenBasePercentage",
  "groundOutsToAirouts"
)

candidate_names[
  candidate_names %in% names(hitters_clean)
]

candidate_names[!candidate_names %in% names(hitters_clean)]

player_features <- hitters_clean

player_features <- player_features |>
  mutate(
    bb_rate = if_else(
      plateAppearances > 0,
      baseOnBalls / plateAppearances,
      NA_real_
    ),
    k_rate = if_else(
      plateAppearances > 0,
      strikeOuts / plateAppearances,
      NA_real_
    ),
    
    hr_rate = if_else(
      plateAppearances > 0,
      homeRuns / plateAppearances,
      NA_real_
    ),
    
    xbh_rate = if_else(
      plateAppearances > 0,
      extraBaseHits / plateAppearances,
      NA_real_
    ),
    
    hbp_rate = if_else(
      plateAppearances > 0,
      hitByPitch / plateAppearances,
      NA_real_
    ),
    
    swing_miss_rate = if_else(
      totalSwings > 0,
      swingAndMisses / totalSwings,
      NA_real_
    ),
    
    stolen_base_attempts =
      stolenBases + caughtStealing,
    
    sb_attempt_rate = if_else(
      plateAppearances > 0,
      stolen_base_attempts / plateAppearances,
      NA_real_
    ),
    
    sb_success_rate = if_else(
      stolen_base_attempts > 0,
      stolenBases / stolen_base_attempts,
      NA_real_
    )
  )

player_features |>
  summarise(
    bb_rate_difference = max(
      abs(bb_rate - walksPerPlateAppearance),
      na.rm = TRUE
    ),
    k_rate_difference = max(
      abs(k_rate - strikeoutsPerPlateAppearance),
      na.rm = TRUE
    ),
    hr_rate_difference = max(
      abs(hr_rate - homeRunsPerPlateAppearance),
      na.rm = TRUE
    )
  )

player_features |>
  select(
    playerName,
    plateAppearances,
    baseOnBalls,
    bb_rate,
    walksPerPlateAppearance,
    strikeOuts,
    k_rate,
    strikeoutsPerPlateAppearance
  ) |>
  head(10)

# Initial set of features to include
initial_model_features <- c(
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

initial_model_features <- intersect(
  initial_model_features,
  names(player_features)
)

initial_model_features

# check stat availability
feature_availability <- tibble(
  feature = initial_model_features,
  missing_count = map_int(
    player_features[initial_model_features],
    ~ sum(is.na(.x))
  ),
  missing_percent = map_dbl(
    player_features[initial_model_features],
    ~ mean(is.na(.x)) * 100
  ),
  unique_values = map_int(
    player_features[initial_model_features],
    n_distinct
  ),
  minimum = map_dbl(
    player_features[initial_model_features],
    ~ min(.x, na.rm = TRUE)
  ),
  maximum = map_dbl(
    player_features[initial_model_features],
    ~ max(.x, na.rm = TRUE)
  )
)

feature_availability

write_csv(feature_availability, "Outputs/Tables/Feature_availability.csv")

modeling_data_provisional <- player_features |> 
  select(all_of(id_columns),
         all_of(initial_model_features))

glimpse(modeling_data_provisional)

# step 12: complete observations
modeling_data_provisional <- modeling_data_provisional |> 
  mutate(complete_model_features = if_all(
    all_of(initial_model_features),
    ~ !is.na(.x)
  ))

modeling_data_provisional |> 
  count(complete_model_features)

# check for variance
feature_variation <- modeling_data_provisional |>
  summarise(
    across(
      all_of(initial_model_features),
      ~ sd(.x, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "feature",
    values_to = "standard_deviation"
  )

feature_variation
# save the data

saveRDS(player_features, "Data/Final/Player_features_engineered.rds")

saveRDS(modeling_data_provisional, "Data/Final/Modeling_data_provisional.rds")

write_csv(modeling_data_provisional, "Data/Final/Modeling_data_provisional.csv")

