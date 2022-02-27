
# I want to first look at if a team covers

library(tidyverse)

pbp <- read_rds("~/pbp_2021.rds")

# https://nflreadr.nflverse.com/articles/dictionary_pbp.html

select(pbp, contains('')) %>% names

spread <- pbp %>%
  filter(!is.na(posteam_type)) %>%
  select(
    game_id,
    home_team, away_team,
    # which team is home/away
    posteam_type,
    result,
    # home_score - away_score, e.g. 
    # negative means away team won, 
    # positive means home team won
    spread_line
    # closing line
    # positive number means the home team was favoured
    # negative number means away team was favoured
  ) %>%
  group_by(game_id) %>%
  # only need 1 row per game since it is the final results
  distinct(game_id, posteam_type, .keep_all = TRUE)

#posteam_is_home
#cover_result

# need a is_home var, so you will want to group by home_team

pbp %>%
  group_by(game_id) %>%
  summarise(
    spread_line = spread_line,
    home_differential = home_score - away_score,
    away_differential = away_score - home_score,
    home_covered = if_else(home_differential > spread_line, 1, 0),
    away_covered = if_else(away_differential > spread_line, 1, 0)
  ) %>%
  distinct(game_id, .keep_all = TRUE) %>% View


pbp %>%
  group_by(game_id) %>%
  summarise(
    spread_line = spread_line,
    home_points = max(home_score),
    away_points = max(away_score),
    home_differential = home_score - away_score,
    away_differential = away_score - home_score,
    home_team_covered = if_else(-1 * spread_line + result > 0, 1, 0),
    away_team_covered = if_else(-1 * spread_line + result > 0, 1, 0)
  ) %>%
  distinct(game_id, .keep_all = TRUE) %>% View


pbp %>%
  group_by(game_id) %>%
  summarise(
    spread_line = spread_line,
    home_team = home_team, 
    away_team = away_team,
    result = result,
    home_points = max(home_score),
    away_points = max(away_score),
    home_differential = home_score - away_score,
    away_differential = away_score - home_score
  ) %>%
  distinct(game_id, .keep_all = TRUE) %>% 
  group_by(game_id, home_team, away_team) %>%
  mutate(
    home_team_covered = if_else(-1 * spread_line + result > 0, 1, 0),
    away_team_covered = if_else(-1 * spread_line + result > 0, 1, 0)
  ) %>% View


spread_df <- pbp %>%
  filter(!is.na(posteam_type)) %>%
  select(game_id, spread_line, result, home_team, away_team, posteam_type) %>%
  distinct(game_id, posteam_type, .keep_all = TRUE)




spread_df %>%
  group_by(home_team) %>%
  mutate(
    home_covered = if_else(-1 * spread_line + result > 0, 1, 0),
    away_covered = if_else(-1 * spread_line + result < 0, 1, 0)
  ) %>%
  ungroup()

spread_df %>%
  filter(spread_line == result)


spread_df %>%
  group_by(home_team) %>%
  mutate(
    home_covered = if_else(-1 * spread_line + result > 0, 1, 0),
    away_covered = if_else(-1 * spread_line + result < 0, 1, 0)
  ) %>%
  ungroup()

#4 games that the spread was a push 
#08_MIA_BUF, 12_CHI_DET, 15_WAS_PHI, 17_ATL_BUF


spread_df1 <- spread_df %>%
  mutate(
    label = dplyr::case_when(
      -1 * spread_line + result > 0 & posteam_type == "home" ~ 1,
      -1 * spread_line + result < 0 & posteam_type == "away" ~ 1,
      TRUE ~ 0
    )
  )

# the only 4 games where the label does not add to 1 are the games
# where the spread was a push
spread_df1 %>%
  group_by(game_id) %>%
  summarise(
    label_sum = sum(label)
  )




