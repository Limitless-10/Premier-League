# Load relevant libraries
library(tidyverse)
library(rstan)
library(worldfootballR)  # For Understat scraping functions
library(shinystan)

# Retrieve all Serie A matches from 1992
seriea_matches <- understat_league_match_results(
  league = "Serie A", 
  season_start_year = 1992
)

stan_mod <- stan_model("BH-Prediction.stan")

unique_teams <- unique(seriea_matches$home_team)

ordered_teams <- sort(unique_teams) 

home_team_index <- match(seriea_matches$home_team, ordered_teams)
away_team_index <- match(seriea_matches$away_team, ordered_teams)

stan_data <- list(
  ngames = nrow(seriea_matches),
  nteams = length(unique_teams),
  hometeam = home_team_index,
  awayteam = away_team_index,
  y1 = seriea_matches$home_goals,
  y2 = seriea_matches$away_goals
)

y1 = seriea_matches$home_goals

fit <- sampling(stan_mod, data = stan_data, chains = 4, core = 4, iter = 2000)

launch_shinystan(fit)











