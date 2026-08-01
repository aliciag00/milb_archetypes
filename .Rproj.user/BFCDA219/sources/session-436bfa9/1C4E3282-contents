install.packages("readr")

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)

# create the API URL
base_url <- "https://bdfed.stitch.mlbinfra.com/bdfed/stats/player"

# Building API request
req <- request(base_url) |>
  req_url_query(
    env = "prod",
    season = 2022,
    stats = "season",
    group = "hitting",
    gameType = "R",
    playerPool = "QUALIFIED",
    leagueIds = 117,
    limit = 1000,
    offset = 0,
    sortStat = "homeRuns",
    order = "desc"
  )

# Send request
response <- req_perform(req)

# read JSON response
json <- resp_body_json(response)

names(json)

length(json$stats)

# player 1
json$stats[[1]]

names(json$stats[[10]])
json$stats[[10]]$playerName
json$stats[[1]]$iso

# Find object type
class(json$stats)

hitters_2022_int <- bind_rows(json$stats)
View(hitters_2022_int)

dim(hitters_2022_int)

tibble(column_number = seq_along(names(hitters_2022_int)),
       column_name = names(hitters_2022_int))

names(hitters_2022_int)[
  grepl(
    "average|avg|onBase|obp|slug|ops|homeRun",
    names(hitters_2022_int),
    ignore.case = TRUE
  )
]

glimpse(hitters_2022_int)

hitters_2022_int |> 
  count(playerId) |> 
  filter(n >1)

# Source Information Columns
hitters_2022_int <- hitters_2022_int |>
  mutate(reqested_season = 2022,
         requested_league_id = 117,
         requested_league = "International League",
         requested_level = "Triple-A")

# Saving first raw CSV
write_csv(
  hitters_2022_int,
  "Data/Raw/International_lg_hitters_2022.csv"
)
  
file.exists(
  "Data/Raw/International_lg_hitters_2022.csv"
)

class(hitters_2022_int)

