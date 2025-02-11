# Load relevant libraries
library(tidyverse)
library(rstan)

## install.packages("devtools")
# devtools::install_github("JaseZiv/worldfootballR")

library(worldfootballR)

team_names <- understat_team_meta(team_name = understat_avaliable_teams(league = 'EPL'))

# Website for data
# https://understat.com/league/EPL

epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2020)
dplyr::glimpse(epl_results)





















