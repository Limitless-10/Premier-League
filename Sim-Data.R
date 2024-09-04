# Load libraries
library(rstan)
library(tidyverse)

#### Simulation of data

# Initialise data
n_teams <- 5

# Play against every other team
n_games_against_each <- 2

n_games <- (n_teams-1)*n_games_against_each

generate_game_data <- function(n_games,n_teams) {
  # Make a fixed schedule (for simplicity)
  rounds <- 1:(n_games/(n_teams-1)) 
  
  
}

# 

