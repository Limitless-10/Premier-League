# Load relevant libraries
library(tidyverse)
library(rstan)
library(worldfootballR) # For most scraping functions

## install.packages("devtools")
# devtools::install_github("JaseZiv/worldfootballR")

# Fetch Arsenal's matches from the 2023/24 EPL season
arsenal_matches <- understat_team_stats_breakdown(
  team_url = "https://understat.com/team/Arsenal/2023"
)

head(arsenal_matches)  # Check the data structure
epl_matches <- understat_league_match_results(league = "EPL", season_start_year = 2023)

arsenal_matches <- epl_matches %>%
  filter(home_team == "Arsenal" | away_team == "Arsenal")

# Display the first few rows
head(arsenal_matches)

# Extract match IDs
match_ids <- arsenal_matches$match_id

# Construct URLs using Understat’s match page format
match_urls <- paste0("https://understat.com/match/", match_ids)

# Initialize an empty list to store goal times
goal_times <- list()
i <- 0

# Loop through each match
for (match_id in match_urls) {
  i <- i + 1
  # Get match details from Understat
  match_data <- understat_match_shots(match_id)
  test_match_players <- understat_match_players(match_id)
  
  # Filter for goals scored by Arsenal
  arsenal_goals <- match_data %>%
    filter(result == "Goal") %>%
    mutate(team = ifelse(home_away == "h", home_team, away_team)) %>%
    select(minute, team, player, home_away, match_id)
  
  # Store in the list
  if (nrow(arsenal_goals > 0)) {
    goal_times[[match_ids[i]]] <- arsenal_goals
  }
}

# Combine into a single dataframe
arsenal_goal_times <- bind_rows(goal_times, .id = "match_id")

game_end_time <- 95
process_match <- function(df_match, default_end_time = 95) {
  
  # Sort events by minute
  df_match <- df_match %>% arrange(minute)
  
  if (nrow(df_match) == 0) {
    return(tibble(
      minute    = default_end_time,
      team      = NA_character_,
      player    = NA_character_,
      home_away = NA_character_,
      match_id  = NA_real_,
      T         = default_end_time,
      T_star    = default_end_time,
      H         = NA_real_,
      C         = 1,
      t_home    = NA_real_,
      t_away    = NA_real_,
      c_home    = default_end_time,
      c_away    = default_end_time
    ))
  }
  
  # Get the max minute from the current match's goal events
  max_goal_minute <- max(df_match$minute, na.rm = TRUE)
  
  # If the last goal minute > default_end_time, use (that minute + 1)
  if (max_goal_minute > default_end_time) {
    this_end_time <- max_goal_minute + 1
  } else {
    this_end_time <- default_end_time
  }
  
  df_match <- df_match %>%
    mutate(
      T      = minute - lag(minute, default = 0),
      T_star = minute, # actual clock time of the event
      H      = ifelse(home_away == "h", 1, 0),  # 1 if home, 0 if away
      C      = 0      # goals are not censoring events
    ) %>%
    mutate(
      t_home = ifelse(H == 1, T, NA_real_),
      t_away = ifelse(H == 0, T, NA_real_),
      c_home = ifelse(H == 1, 0, T),   # censoring time for home if away scored
      c_away = ifelse(H == 0, 0, T)    # censoring time for away if home scored
    )
  
  last_minute     <- max(df_match$minute)
  censor_interval <- this_end_time - last_minute
  
  censor_row <- tibble(
    minute    = this_end_time,
    team      = NA_character_,
    player    = NA_character_,
    home_away = NA_character_,
    match_id  = unique(df_match$match_id),
    T         = censor_interval,
    T_star    = this_end_time,
    # We'll carry forward the last event's H or set to NA if you prefer
    H         = if (nrow(df_match) > 0) last(df_match$H) else NA_real_,
    C         = 1,  # This indicates it's a censoring row
    t_home    = NA_real_,
    t_away    = NA_real_,
    c_home    = censor_interval,
    c_away    = censor_interval
  )
  
  # Combine goal events + final censoring row
  df_out <- bind_rows(df_match, censor_row)
  
  return(df_out)
}

# Apply the processing function to each match in your arsenal_goal_times data.
converted_data <- arsenal_goal_times %>%
  group_by(match_id) %>%
  group_modify(~ process_match(.x, game_end_time)) %>%
  ungroup()

# View the new (converted) data
print(converted_data)

### Fit the stan model

stan_data <- list(y=converted_data[, c("t_home", "t_away", "c_home", "c_away")],
                  N = nrow(converted_data))

stanmod <- stan_model("SurvGoal.stan")

fit <- sampling(stanmod, data = stan_data, chains = 4, iter = 2000)


######################## Commands maybe worth remembering

# # team_names <- understat_team_meta(team_name = understat_avaliable_teams(league = 'EPL'))
# team_urls <- understat_team_meta(team_name = c("Liverpool", "Manchester City"))




