# Load relevant libraries
library(tidyverse)
library(rstan)
library(shinystan)

stan_mod <- stan_model("Premier-League.stan")



# nteams; // number of teams (20)
# ngames; // number of games
# nweeks; // number of weeks
# home_week[ngames]; // week number for the home team
# away_week[ngames]; // week number for the away team
# home_team[ngames]; // home team ID (1, ..., 20)
# away_team[ngames]; // away team ID (1, ..., 20)
# score_diff; // home_goals - away_goals
# prev_perf; // a score between -1 and +1




















