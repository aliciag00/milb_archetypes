library(httr2)
library(dplyr)
library(tibble)
library(readr)

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
  
  hitters <- bind_rows(json$stats) |> 
    mutate(
      requested_season = season,
      requested_league_id = league_id,
      requested_league_name = league_name,
      requested_level = level
      )
  
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
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
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
