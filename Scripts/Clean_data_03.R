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
