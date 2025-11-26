library(tidyverse)
library(stringr)

# Ensure 'data' directory exists (relative to working directory)
if (!dir.exists("data")) {
  dir.create("data")
}

# Download KenPom data
url <- "https://kenpom.com/cbbga26.txt"
raw_lines <- read_lines(url)

# Function to parse each line into a row
parse_game <- function(line) {
  # Ignore blank or very short lines
  if (nchar(str_trim(line)) < 25) return(NULL)
  # Date is always first 10 characters
  date <- str_sub(line, 1, 10)
  rest <- str_trim(str_sub(line, 12))
  # Regular expression to extract: away team, away points, home team, home points
  m <- str_match(rest, "^(.+?)\\s+(\\d+)\\s+(.+?)\\s+(\\d+)\\s*")
  if (is.na(m[1, 1])) return(NULL)
  away_points <- as.numeric(m[1, 3])
  home_points <- as.numeric(m[1, 5])
  tibble(
    date = date,
    away_points = away_points,
    home_points = home_points
  )
}

# Parse all lines and drop incomplete/null rows
games <- map_df(raw_lines, parse_game) %>%
  drop_na(away_points, home_points)

daily_summary <- games %>%
  group_by(date) %>%
  summarise(
    total_away_points = sum(away_points, na.rm = TRUE),
    total_home_points = sum(home_points, na.rm = TRUE),
    num_games = n(),
    avg_points_per_game = (total_away_points + total_home_points) / num_games,
    .groups = "drop"
  )

# Daily summary calculation
daily_summary <- daily_summary %>% 
  mutate(
    total_away_points = as.numeric(total_away_points),
    total_home_points = as.numeric(total_home_points),
    num_games = as.integer(num_games),
    avg_points_per_game = as.numeric(avg_points_per_game)
  )

# File path for output in 'data' folder
csv_file <- file.path("data", "kenpom_daily_summary.csv")

# Load existing summary CSV if present, append new rows
if (file.exists(csv_file)) {
  prev <- read_csv(csv_file, show_col_types = FALSE) %>%
    mutate(
      total_away_points = as.numeric(total_away_points),
      total_home_points = as.numeric(total_home_points),
      num_games = as.integer(num_games),
      avg_points_per_game = as.numeric(avg_points_per_game)
    )
  new_rows <- anti_join(daily_summary, prev, by = "date")
  updated <- bind_rows(prev, new_rows)
} else {
  updated <- daily_summary
}

# Save updated file in 'data' folder
write_csv(updated, csv_file)
