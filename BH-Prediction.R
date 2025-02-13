# Load relevant libraries
library(tidyverse)
library(rstan)
library(worldfootballR)  # For Understat scraping functions

# =====================================================
# 1. Get Roma's Match Data for the 1992/93 Serie A Season
# =====================================================

# (a) Option 1: Get Roma-specific breakdown (if available)
roma_breakdown <- understat_team_stats_breakdown(
  team_url = "https://understat.com/team/Roma/1992"  # use the appropriate URL/season
)

# (b) Option 2: Get all Serie A match results for season 1992 and then filter for Roma
seriea_matches <- understat_league_match_results(
  league = "Serie A", 
  season_start_year = 1992
)

roma_matches <- seriea_matches %>%
  filter(home_team == "Roma" | away_team == "Roma")

# Check the structure of Roma's matches
head(roma_matches)

# =====================================================
# 2. Extract Goal Events for Each Match
# =====================================================

# Extract the match IDs from the filtered matches
match_ids <- roma_matches$match_id

# Build match URLs using Understat’s match page format
match_urls <- paste0("https://understat.com/match/", match_ids)

# Initialize an empty list to store goal events from each match
goal_events <- list()
i <- 0

for (match_url in match_urls) {
  i <- i + 1
  
  # Get match shot data from Understat
  match_data <- understat_match_shots(match_url)
  
  # For each match, extract only the goal events and determine whether the goal was scored or conceded by Roma.
  # Note: understat_match_shots returns (among other columns) the following:
  #   - 'result' (e.g. "Goal"),
  #   - 'minute' (minute of the event),
  #   - 'home_away' ("h" if the shot is by the home team, "a" if away),
  #   - 'home_team' and 'away_team'.
  match_goals <- match_data %>%
    filter(result == "Goal") %>%
    mutate(
      scoring_team = ifelse(home_away == "h", home_team, away_team),
      goal_type = ifelse(scoring_team == "Roma", "scored", "conceded")
    ) %>%
    select(minute, scoring_team, goal_type, home_away, match_id)
  
  # Only add if there is at least one goal event.
  if (nrow(match_goals) > 0) {
    goal_events[[ match_ids[i] ]] <- match_goals
  }
}

# Combine all matches into one data frame
roma_goal_events <- bind_rows(goal_events, .id = "match_id")
head(roma_goal_events)

# =====================================================
# 3. (Optional) Process the Event Data for Survival Modeling
#     (Following your original approach with censoring rows, etc.)
# =====================================================

# Define the end-of-game time (you can adjust if needed)
game_end_time <- 95

process_match <- function(df_match, default_end_time = 95) {
  
  # Ensure events are in chronological order
  df_match <- df_match %>% arrange(minute)
  
  # If no goal events occurred in the match, create a censoring row only
  if (nrow(df_match) == 0) {
    return(tibble(
      minute    = default_end_time,
      scoring_team = NA_character_,
      goal_type = NA_character_,
      home_away = NA_character_,
      match_id  = NA_character_,
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
  
  # Determine the effective game end time (in case the last event occurs after the default)
  max_goal_minute <- max(df_match$minute, na.rm = TRUE)
  this_end_time <- ifelse(max_goal_minute > default_end_time, max_goal_minute + 1, default_end_time)
  
  df_match <- df_match %>%
    mutate(
      T      = minute - lag(minute, default = 0),
      T_star = minute,
      H      = ifelse(home_away == "h", 1, 0),  # indicator: 1 if goal by home team, 0 if away
      C      = 0  # not a censoring event
    ) %>%
    mutate(
      t_home = ifelse(H == 1, T, NA_real_),
      t_away = ifelse(H == 0, T, NA_real_),
      c_home = ifelse(H == 1, 0, T),   # censoring time for home when away scores
      c_away = ifelse(H == 0, 0, T)    # censoring time for away when home scores
    )
  
  last_minute     <- max(df_match$minute)
  censor_interval <- this_end_time - last_minute
  
  # Create a final censoring row for the match
  censor_row <- tibble(
    minute    = this_end_time,
    scoring_team = NA_character_,
    goal_type = NA_character_,
    home_away = NA_character_,
    match_id  = unique(df_match$match_id),
    T         = censor_interval,
    T_star    = this_end_time,
    H         = if (nrow(df_match) > 0) last(df_match$H) else NA_real_,
    C         = 1,  # indicates censoring
    t_home    = NA_real_,
    t_away    = NA_real_,
    c_home    = censor_interval,
    c_away    = censor_interval
  )
  
  bind_rows(df_match, censor_row)
}

# Apply the processing function to each match’s events
converted_data <- roma_goal_events %>%
  group_by(match_id) %>%
  group_modify(~ process_match(.x, default_end_time = game_end_time)) %>%
  ungroup()

# Inspect the processed data
print(converted_data)