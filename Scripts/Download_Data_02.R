# install.packages("tidyr")

library(httr2)
library(dplyr)
library(tibble)
library(readr)
library(purrr)
library(tidyr)

# Base URL
base_url <- "https://bdfed.stitch.mlbinfra.com/bdfed/stats/player"

# Function shell
get_milb_hitters <- function(
    season,
    league_id,
    league_name,
    level
    ) {
  
  # Adding error protection
  if(length(season) != 1 || is.na(season)) {
    stop("season must be one non-missing value.")
  }
  
  if(length(league_id) != 1 || is.na(league_id)) {
    stop("league_id must be one non-missing value.")
  }
  
  message(
    "Downloading ",
    league_name,
    " - ",
    season
  )
  req <- request(base_url) |>
    req_url_query(
      env = "prod",
      season = season,
      stats = "season",
      group = "hitting",
      gameType = "R",
      playerPool = "QUALIFIED",
      leagueIds = league_id,
      limit = 1000,
      offset = 0,
      sortStat = "homeRuns",
      order = "desc"
    )
  
  response <- req |> 
    req_retry(max_tries = 3) |> 
    req_perform()
  
  json <- resp_body_json(
    response,
    simplifyVector = FALSE
  )
  
  if(length(json$stats) == 0) {
    warning(
      "No data returned for ",
      league_name,
      " - ",
      season
    )
    
    return(
      requested_season = season,
      requested_league_id = league_id,
      requested_league_name = league_name,
      requested_level = level
    )
  }
  hitters <- bind_rows(json$stats) |> 
    mutate(
      requested_season = season,
      requested_league_id = league_id,
      requested_league_name = league_name,
      requested_level = level
      )
  
  Sys.sleep(0.25)
  
  hitters
}

test_hitters <- get_milb_hitters(
  season = 2022,
  league_id = 117,
  league_name = "International League",
  level = "Triple-A"
)

nrow(test_hitters)

# Part 2
# make function able to grab different leagues / years

league_reference <- tibble(
  league_name = c(
    "International League",
    "Pacific Coast League",
    "Eastern League",
    "Southern League",
    "Texas League",
    "Midwest League",
    "Northwest League",
    "South Atlantic League",
    "California League",
    "Carolina League",
    "Florida State League"
  ),
  level = c(
    "Triple-A",
    "Triple-A",
    "Double-A",
    "Double-A",
    "Double-A",
    "High-A",
    "High-A",
    "High-A",
    "Low-A",
    "Low-A",
    "Low-A"
  ),
  league_id = c(
    117,
    112,
    113,
    111,
    109,
    118,
    126,
    116,
    110,
    122,
    123
  )
)

league_reference

pcl_2022 <- get_milb_hitters(
  season = 2022,
  league_id = league_reference$league_id[
    league_reference$league_name == "Pacific Coast League"
  ],
  league_name = "Pacific Coast League",
  level = "Triple-A"
)

nrow(pcl_2022)  

pcl_2022 |> 
  distinct(
    leagueName,
    leagueId,
    requested_league_id
  )

pcl_2022 |> 
  distinct(teamId, teamName) |> 
  arrange(teamName)

two_league_test <- bind_rows(
  international_league = test_hitters,
  pacific_coast_league = pcl_2022,
  .id = "download_source"
)

two_league_test |> 
  count(requested_season,
        requested_league_id,
        leagueName,
        download_source,
        name = "qualified_hitters")

nrow(test_hitters) > 0

league_reference

seasons <- 2022:2025

seasons

# Create every season-lg combo
download_plan <- crossing(
  season = seasons,
  league_reference
)

download_plan
nrow(download_plan)

head(download_plan)

# test on three leagues first
test_plan <- download_plan |> 
  slice(1:3)

test_plan

test_results <- pmap(
  test_plan,
  function(season, league_name, level, league_id) {
    get_milb_hitters(
      season = season,
      league_id = league_id,
      league_name = league_name,
      level = level
    )
  }
)

class(test_results)

map_int(test_results, nrow)

test_combined <- list_rbind(test_results)
dim(test_combined)

test_combined |> 
  distinct(
    requested_season,
    requested_league_id,
    requested_league_name,
    requested_level
  )

nrow(test_combined) == sum(map_int(test_results, nrow))

all_results <- pmap(
  download_plan,
  function(season, league_name, level, league_id) {
    get_milb_hitters(
      season = season,
      league_id = league_id,
      league_name = league_name,
      level = level
    )
  }
)

length(all_results)

request_counts <- download_plan |> 
  mutate(
    qualified_hitters = map_int(all_results, nrow)
  )
request_counts

request_counts |> 
  filter(qualified_hitters == 0)

summary(request_counts$qualified_hitters)

# Create raw data - all leagues and years
all_hitters_raw <- list_rbind(all_results)
dim(all_hitters_raw)

all_hitters_raw |> 
  count(requested_season, name = "player_seasons")

all_hitters_raw |> 
  count(requested_season, requested_level, name = "player_seasons") |> 
  arrange(requested_season, requested_level)

# check for duplicates
duplicate_check <- all_hitters_raw |> 
  count(requested_season, requested_league_id, playerId) |> 
  filter(n > 1)
duplicate_check

# Save the full raw dataset
dir.create("Data/Raw",
           recursive =  TRUE,
           showWarnings = FALSE)

write_csv(all_hitters_raw,
          "Data/Raw/Milb_qualified_hitters_2022_2025.csv")

write_csv(request_counts,
          "Data/Raw/Download_audit_2022_2025.csv")

file.exists("Data/Raw/Milb_qualified_hitters_2022_2025.csv")
file.exists("Data/Raw/Download_audit_2022_2025.csv")

saveRDS(all_hitters_raw,
        "Data/Raw/Milb_qualified_hitters_2022_2025.rds")

file.info(c(
  "Data/Raw/Milb_qualified_hitters_2022_2025.csv",
  "Data/Raw/Milb_qualified_hitters_2022_2025.csv"
))$size
