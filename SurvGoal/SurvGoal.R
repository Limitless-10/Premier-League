# Load relevant libraries
library(tidyverse)
library(rstan)
library(worldfootballR)

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

# For demonstration, assume every game ends at 95 minutes.
# (In practice, you might merge in a game_end column from your match metadata.)
game_end_time <- 95

# Define a function that processes one match’s goal events.
process_match <- function(df_match, game_end_time) {
  # Ensure events are sorted by minute
  df_match <- df_match %>% arrange(minute)
  
  print(df_match)
  
  # Compute the time interval T.
  # For the first event, T equals the minute; for later events, it is the difference from the previous event.
  df_match <- df_match %>%
    mutate(T = minute - lag(minute, default = 0),
           T_star = minute)  # T_star is the actual event time
  
  # Create the home indicator: H = 1 if Arsenal was at home (home_away == "h"), 0 if away.
  df_match <- df_match %>%
    mutate(H = ifelse(home_away == "h", 1, 0),
           C = 0)  # Actual goal events are not censoring events
  
  # Now assign goal arrival times and censoring times.
  df_match <- df_match %>%
    mutate(
      t_home = ifelse(H == 1, T, NA_real_),
      t_away = ifelse(H == 0, T, NA_real_),
      c_home = ifelse(H == 1, 0, T),
      c_away = ifelse(H == 0, 0, T)
    )
  
  # Compute the final censoring (end-of-game) interval.
  last_minute <- max(df_match$minute)
  censor_interval <- game_end_time - last_minute
  
  # Create a final row for censoring.
  censor_row <- tibble(
    minute    = game_end_time,
    team      = NA_character_,
    player    = NA_character_,
    home_away = NA_character_,
    match_id  = unique(df_match$match_id),
    T         = censor_interval,
    T_star    = game_end_time,
    # Here we simply carry forward the last event's H (this choice may be adjusted)
    H         = if(nrow(df_match) > 0) last(df_match$H) else NA_real_,
    C         = 1,   # This is a censoring observation.
    t_home    = NA_real_,
    t_away    = NA_real_,
    c_home    = censor_interval,
    c_away    = censor_interval
  )
  
  # Bind the goal events and the censoring row.
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




######################## Commands maybe worth remembering

# # team_names <- understat_team_meta(team_name = understat_avaliable_teams(league = 'EPL'))
# team_urls <- understat_team_meta(team_name = c("Liverpool", "Manchester City"))




