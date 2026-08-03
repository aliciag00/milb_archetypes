library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(purrr)

all_hitters_raw <- readRDS("Data/Raw/Milb_qualified_hitters_2022_2025.rds")

class(all_hitters_raw)
dim(all_hitters_raw)

all_hitters_raw |> 
  glimpse()

# raw dataset:
# Rows: 2,016
# Columns: 78

# Unit of Observation:
# One qualified hitter-league-season record

column_inventory <- tibble(
  column_number = seq_along(names(all_hitters_raw)),
  column_name = names(all_hitters_raw),
  data_type = map_chr(
    all_hitters_raw,
    ~ paste(class(.x), collapse = ", ")
  ),
  missing_count = map_int(
    all_hitters_raw,
    ~ sum(is.na(.x))
  ),
  unique_values = map_int(
    all_hitters_raw,
    dplyr::n_distinct
  )
)

View(column_inventory)

dir.create("Outputs/Tables",
           recursive = TRUE,
           showWarnings = FALSE)

write_csv(column_inventory, "Outputs/Tables/raw_column_inventory.csv")

all_hitters_raw |> 
  select(playerId, playerName,
         requested_season,
         requested_league_id,
         requested_league_name,
         requested_level,
         teamId,
         teamName) |> 
  head(10)

all_hitters_raw |> 
  count(requested_season)

all_hitters_raw |> 
  count(requested_level)

all_hitters_raw |> 
  count(requested_league_name)

duplicate_records <- all_hitters_raw |> 
  count(
    playerId,
    requested_season,
    requested_league_id,
    name = "record_count"
  ) |> 
  filter(record_count > 1)
duplicate_records

# Check for players in multiple leagues in one season
multi_league_player_seasons <- all_hitters_raw |>
  distinct(
    playerId,
    playerName,
    requested_season,
    requested_league_id,
    requested_league_name,
    requested_level
  ) |>
  count(
    playerId,
    playerName,
    requested_season,
    name = "league_count"
  ) |>
  filter(league_count > 1)

nrow(multi_league_player_seasons)
  
missing_summary <- all_hitters_raw |> 
  summarise(
    across(
      everything(), ~ sum(is.na(.x))
    )
  ) |> 
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "missing_count"
  ) |> 
  mutate(missing_percent = missing_count / nrow(all_hitters_raw) * 100) |> 
  arrange(desc(missing_percent))

missing_summary |> 
  filter(missing_count >0)

# Some of these results will need to be changed into numeric values
all_hitters_raw |> 
  select(
    where(is.character)
  ) |> 
  names()

all_hitters_raw |> 
  summarise(
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    min_pa = min(plateAppearances, na.rm = TRUE),
    max_pa = max(plateAppearances, na.rm = TRUE),
    min_hr = min(homeRuns, na.rm = TRUE),
    max_hr = max(homeRuns, na.rm = TRUE)
  )

# Audit Summary
audit_summary <- tibble(
  metric = c(
    "rows",
    "columns",
    "unique_players",
    "seasons",
    "leagues",
    "levels",
    "duplicate_record_keys",
    "multi_league_player_seasons"
  ),
  value = c(
    nrow(all_hitters_raw),
    ncol(all_hitters_raw),
    n_distinct(all_hitters_raw$playerId),
    n_distinct(all_hitters_raw$requested_season),
    n_distinct(all_hitters_raw$requested_league_id),
    n_distinct(all_hitters_raw$requested_level),
    nrow(duplicate_records),
    nrow(multi_league_player_seasons)
  )
)

audit_summary

write_csv(audit_summary, "Outputs/Tables/raw_data_audit_summary.csv")

# Data Cleaning

hitters_clean <- all_hitters_raw

character_columns <- hitters_clean |> 
  select(
    where(is.character)) |> 
  names()

character_columns

rate_columns <- c("babip",
                  "pitchesPerPlateAppearance",
                  "walksPerPlateAppearance",
                  "strikeoutsPerPlateAppearance",
                  "homeRunsPerPlateAppearance",
                  "walksPerStrikeout",
                  "iso",
                  "avg",
                  "obp",
                  "slg",
                  "ops",
                  "stolenBasePercentage",
                  "caughtStealingPercentage",
                  "groundOutsToAirouts",
                  "atBatsPerHomeRun"
                  )


rate_columns <- intersect(
  rate_columns, names(hitters_clean)
)

rate_columns

map(hitters_clean[rate_columns],
    ~ head(unique(.x), 10))


hitters_clean <- hitters_clean |> 
  mutate(
    across(where(is.character),
           ~ na_if(trimws(.x), "")
  ))

placeholder_values <- c(
  ".---",
  "-.--",
  "---",
  "-",
  "N/A",
  "NA",
  ""
)

hitters_clean <- hitters_clean |>
  mutate(
    across(
      all_of(rate_columns),
      ~ case_when(
        .x %in% placeholder_values ~ NA_character_,
        TRUE ~ .x
      )
    )
  ) |>
  mutate(
    across(
      all_of(rate_columns),
      readr::parse_number
    )
  )

hitters_clean |> 
  select(all_of(rate_columns)) |> 
  glimpse()

# compare old and new values
tibble(original_iso = all_hitters_raw$iso,
       cleaned_iso = hitters_clean$iso) |> 
  tail(10)

conversion_audit <- tibble(
  column_name = rate_columns,
  
  missing_before = map_int(
    all_hitters_raw[rate_columns],
    ~ sum(is.na(.x))
  ),
  
  placeholder_count = map_int(
    all_hitters_raw[rate_columns],
    ~ sum(.x %in% placeholder_values, na.rm = TRUE)
  ),
  
  missing_after = map_int(
    hitters_clean[rate_columns],
    ~ sum(is.na(.x))
  )
) |>
  mutate(
    unexplained_missing =
      missing_after -
      missing_before -
      placeholder_count
  )

conversion_audit

integer_candidates <- c(
  "year",
  "playerId",
  "teamId",
  "leagueId",
  "age",
  "plateAppearances",
  "totalBases",
  "extraBaseHits",
  "homeRuns",
  "walks",
  "strikeouts",
  "stolenBases",
  "caughtStealing",
  "hits",
  "doubles",
  "triples",
  "atBats",
  "gamesPlayed"
)

integer_candidates <- c("year",
                        "playerId",
                        "teamId",
                        "leagueId",
                        "age",
                        "plateAppearances",
                        "totalBases",
                        "extraBaseHits",
                        "homeRuns",
                        "walks",
                        "strikeouts",
                        "stolenBases",
                        "caughtStealing",
                        "hits",
                        "doubles",
                        "triples",
                        "atBats",
                        "gamesPlayed")

integer_candidates <- intersect(
  integer_candidates,
  names(hitters_clean)
)

tibble(column_name = integer_candidates,
       data_type = map_chr(
         hitters_clean[integer_candidates],
         ~ paste(class(.x), collapse = ", ")
       ))

integer_candidates_columns <- integer_candidates[map_lgl(
  hitters_clean[integer_candidates],
  is.character
)]

integer_candidates_columns

if(length(integer_candidates_columns) >0) {
  hitters_clean <- hitters_clean |> 
    mutate(
      across(all_of(integer_candidates_columns),
             readr::parse_integer)
    )
}

# step 11
hitters_clean |> 
  distinct(year, requested_season)

# step 12
league_validation <- hitters_clean |> 
  distinct(
    leagueId,
    leagueName,
    requested_league_id,
    requested_league_name,
    requested_level
  ) |> 
  arrange(
    requested_level, requested_league_name
  )

league_validation

hitters_clean |> 
  filter(leagueId != requested_league_id) |> 
  distinct(leagueId, leagueName, requested_league_id, requested_league_name)

# step 13
hitters_clean <- hitters_clean |> 
  mutate(player_league_season_id = paste(
    playerId, requested_season, requested_league_id, sep = "-"
  ))

# Player identification table
player_identification <- hitters_clean |>
  select(
    any_of(
      c(
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
        "age"
      )
    )
  ) |>
  distinct()

nrow(player_identification)

player_identification |> 
  count(player_league_season_id) |> 
  filter(n >1)

clean_missing_summary <- hitters_clean |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "column_name",
    values_to = "missing_count"
  ) |>
  mutate(
    missing_percent =
      missing_count / nrow(hitters_clean) * 100
  ) |>
  filter(missing_count > 0) |>
  arrange(desc(missing_percent))

clean_missing_summary

# Save the cleaned data
saveRDS(hitters_clean, "Data/Cleaned/Milb_hitters_clean.rds")

write_csv(hitters_clean, "Data/Cleaned/Milb_hitters_clean.csv", na = "")

saveRDS(player_identification, "Data/Cleaned/Player_identification.rds")

write_csv(conversion_audit, "Outputs/Tables/Rate_column_conversion_audit.csv")

write_csv(clean_missing_summary, "Outputs/Tables/Cleaned_missingness_summary.csv")
