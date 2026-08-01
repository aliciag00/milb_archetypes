library(httr2)
library(dplyr)
library(tibble)
library(readr)

# Base URL
base_url <- "https://bdfed.stitch.mlbinfra.com/bdfed/stats/player"

# Function shell
get_milb_hitters <- function(season, league_id) {
  
  # Adding error protection
  if(length(season) != 1 || is.na(season)) {
    stop("season must be one non-missing value.")
  }
  
  if(length(league_id) != 1 || is.na(league_id)) {
    stop("league_id must be one non-missing value.")
  }
  
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
      requested_league_id = league_id
      )
  
  hitters
}

test_hitters <- get_milb_hitters(
  season = 2022,
  league_id = 117
)

nrow(test_hitters)
