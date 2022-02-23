

#Rolling Averages of EPA

#https://twitter.com/greerreNFL/status/1343830333783822338

#find every team’s weekly EPA/play on offense and defense, breaking it down into passing and rushing EPA/play 


weekly_epa_play <- function(pbp) {
  # Offense EPA
  pbp <- pbp %>%
    dplyr::filter(
      !is.na(.data$epa),
      !is.na(.data$ep),
      !is.na(.data$posteam),
      .data$play_type %in% c("pass", "run"),
      .data$qb_kneel != 1
    ) %>%
    dplyr::group_by(.data$game_id, .data$season, .data$week, .data$posteam, .data$home_team) %>%
    dplyr::summarise(
      off_dropback_pct = mean(.data$qb_dropback == 1),
      off_epa = mean(.data$epa),
      off_pass_epa = mean(.data$epa[.data$qb_dropback == 1]),
      off_rush_epa = mean(.data$epa[.data$qb_dropback == 0]),
      off_epa_n = sum(.data$qb_dropback == 1 | .data$qb_dropback == 0),
      off_pass_epa_n = sum(.data$qb_dropback == 1),
      off_rush_epa_n = sum(.data$qb_dropback == 0),
      .groups = "drop"
    ) %>%
    # Defense EPA
    dplyr::left_join(
      filter(
        pbp,
        !is.na(.data$epa),
        !is.na(.data$ep),
        !is.na(.data$posteam),
        .data$play_type %in% c("pass", "run"),
        .data$qb_kneel != 1
      ) %>%
        dplyr::group_by(.data$game_id, .data$season, .data$week, .data$defteam, .data$away_team) %>%
        dplyr::summarise(
          def_epa = mean(.data$epa),
          def_dropback_pct = mean(.data$qb_dropback == 1),
          def_pass_epa = mean(.data$epa[.data$qb_dropback == 1]),
          def_rush_epa = mean(.data$epa[.data$qb_dropback == 0]),
          def_epa_n = sum(.data$qb_dropback == 1 | .data$qb_dropback == 0),
          def_pass_epa_n = sum(.data$qb_dropback == 1),
          def_rush_epa_n = sum(.data$qb_dropback == 0),
          .groups = "drop"
        ),
      by = c("game_id", "posteam" = "defteam", "season", "week")
    ) %>%
    dplyr::mutate(opponent = dplyr::if_else(.data$posteam == .data$home_team, .data$away_team, .data$home_team)) %>%
    dplyr::select(
      .data$game_id, .data$season, .data$week, .data$home_team, .data$away_team, .data$posteam,
      .data$opponent, .data$off_dropback_pct, .data$off_epa, .data$off_pass_epa, .data$off_rush_epa,
      .data$off_epa_n, .data$off_pass_epa_n, .data$off_rush_epa_n, .data$def_epa_n, .data$def_pass_epa_n,
      .data$def_rush_epa_n, .data$def_dropback_pct, .data$def_epa, .data$def_pass_epa, .data$def_rush_epa
    )
  
  return(pbp)
}



# Convert each EPA statistic into a lagging moving average. 
# The lag ensures that we compare a team’s performance against their opponent’s performance up to that point in the season.

# Instead of weighting each game equally during the window, we will weight EPA by the number of plays in each game of the window.

# Instead of simply converting each statistic into a moving average of the last ten games, we will convert each statistic into a moving average 
#using a dynamic window that ranges from ten games to twenty games (for teams that play in the Super Bowl). 

#In other words, we will use a ten game window to predict the winner of the 11th game, but for say the 15th game, we will use a 14 game window.

# Note that for new seasons, this ten game window serves as a prior for each team in a similar manner to Football Outsiders’ weighted DVOA.


# Function to get moving average of a dynamic window from 10 - 20 games
wt_mov_avg_local <- function(var, weight, window, type, moving = TRUE) {
  if (length(weight) == 1 & weight[1] == 1) {
    weight <- rep(1, length(var))
  }
  if (moving) {
    dplyr::case_when(
      window == 10 ~ pracma::movavg(var * weight, n = 10, type = type) /
        pracma::movavg(weight, n = 10, type = type),
      window == 11 ~ pracma::movavg(var * weight, n = 11, type = type) /
        pracma::movavg(weight, n = 11, type = type),
      window == 12 ~ pracma::movavg(var * weight, n = 12, type = type) /
        pracma::movavg(weight, n = 12, type = type),
      window == 13 ~ pracma::movavg(var * weight, n = 13, type = type) /
        pracma::movavg(weight, n = 13, type = type),
      window == 14 ~ pracma::movavg(var * weight, n = 14, type = type) /
        pracma::movavg(weight, n = 14, type = type),
      window == 15 ~ pracma::movavg(var * weight, n = 15, type = type) /
        pracma::movavg(weight, n = 15, type = type),
      window == 16 ~ pracma::movavg(var * weight, n = 16, type = type) /
        pracma::movavg(weight, n = 16, type = type),
      window == 17 ~ pracma::movavg(var * weight, n = 17, type = type) /
        pracma::movavg(weight, n = 17, type = type),
      window == 18 ~ pracma::movavg(var * weight, n = 18, type = type) /
        pracma::movavg(weight, n = 18, type = type),
      window == 19 ~ pracma::movavg(var * weight, n = 19, type = type) /
        pracma::movavg(weight, n = 19, type = type),
      window == 20 ~ pracma::movavg(var * weight, n = 20, type = type) /
        pracma::movavg(weight, n = 20, type = type)
    )
  } else {
    pracma::movavg(var * weight, n = 10, type = type) /
      pracma::movavg(weight, n = 10, type = type)
  }
}

# Function to create the dataset
create_rolling_data <- function(pbp, move = TRUE, team_n = 10L, pt_diff_type = "r", epa_off = "e",
                                epa_pass_o = "e", epa_rush_o = "r", epa_def = "s", epa_pass_d = "s",
                                epa_rush_d = "r", epa_drop = "r") {
  
  # Join back opponent off/def EPA
  pbp <- pbp %>%
    dplyr::group_by(.data$season, .data$posteam) %>%
    dplyr::mutate(game_number = row_number()) %>%
    dplyr::left_join(pbp %>%
                       dplyr::group_by(.data$season, .data$posteam) %>%
                       dplyr::mutate(opp_game_number = row_number()) %>%
                       dplyr::select(-.data$opponent) %>%
                       dplyr::rename(
                         opp_off_epa = off_epa,
                         opp_off_pass_epa = off_pass_epa,
                         opp_off_rush_epa = off_rush_epa,
                         opp_def_epa = def_epa,
                         opp_def_pass_epa = def_pass_epa,
                         opp_def_rush_epa = def_rush_epa,
                         opp_off_epa_n = off_epa_n,
                         opp_off_pass_epa_n = off_pass_epa_n,
                         opp_off_rush_epa_n = off_rush_epa_n,
                         opp_def_epa_n = def_epa_n,
                         opp_def_pass_epa_n = def_pass_epa_n,
                         opp_def_rush_epa_n = def_rush_epa_n,
                         opp_off_dropback_pct = off_dropback_pct,
                         opp_def_dropback_pct = def_dropback_pct
                       ) %>%
                       dplyr::group_by(.data$posteam) %>%
                       dplyr::arrange(.data$season, .data$week) %>%
                       dplyr::mutate(
                         window = dplyr::if_else(.data$opp_game_number <= 10, team_n, .data$opp_game_number),
                         # Opponent off EPA
                         opp_off_epa = dplyr::lag(wt_mov_avg_local(var = .data$opp_off_epa, weight = .data$opp_off_epa_n, window = .data$window, type = epa_off, moving = move)),
                         opp_off_pass_epa = dplyr::lag(wt_mov_avg_local(var = .data$opp_off_pass_epa, weight = .data$opp_off_pass_epa_n, window = .data$window, type = epa_pass_o, moving = move)),
                         opp_off_rush_epa = dplyr::lag(wt_mov_avg_local(var = .data$opp_off_rush_epa, weight = .data$opp_off_rush_epa_n, window = .data$window, type = epa_rush_o, moving = move)),
                         # Opponent def EPA
                         opp_def_epa = dplyr::lag(wt_mov_avg_local(var = .data$opp_def_epa, weight = .data$opp_def_epa_n, window = .data$window, type = epa_def, moving = move)),
                         opp_def_pass_epa = dplyr::lag(wt_mov_avg_local(var = .data$opp_def_pass_epa, weight = .data$opp_def_pass_epa_n, window = .data$window, type = epa_pass_d, moving = move)),
                         opp_def_rush_epa = dplyr::lag(wt_mov_avg_local(var = .data$opp_def_rush_epa, weight = .data$opp_def_rush_epa_n, window = .data$window, type = epa_rush_d, moving = move)),
                         # Opponent defense dropbacks
                         opp_def_dropback_pct = dplyr::lag(wt_mov_avg_local(var = .data$opp_def_dropback_pct, weight = 1, window = .data$window, type = epa_drop, moving = move)),
                         # Opponent offense dropbacks
                         opp_off_dropback_pct = dplyr::lag(wt_mov_avg_local(var = .data$opp_off_dropback_pct, weight = 1, window = .data$window, type = epa_drop, moving = move))
                       ),
                     by = c("game_id", "season", "week", "home_team", "away_team", "opponent" = "posteam")
    ) %>%
    # Fix errors that occur for "1999_01_BAL_STL", "2000_06_BUF_MIA", "2000_03_SD_KC" games (NAs)
    dplyr::mutate(dplyr::across(
      c(.data$opp_off_dropback_pct:.data$opp_off_rush_epa, .data$opp_def_dropback_pct:.data$opp_def_rush_epa),
      ~ dplyr::if_else(is.na(.) & .data$week != 1, 0, .)
    ))
  
  # Join in league mean of EPA by season, week to prepare for opponent adjustments
  pbp <- pbp %>%
    dplyr::left_join(
      dplyr::filter(
        pbp,
        .data$posteam == .data$home_team
      ) %>%
        dplyr::group_by(.data$season, .data$week) %>%
        dplyr::summarise(
          league_mean_total = sum(.data$off_epa * .data$off_epa_n + .data$def_epa * .data$def_epa_n) / sum(.data$off_epa_n + .data$def_epa_n),
          league_mean_dropback_pct = (mean(.data$off_dropback_pct) + mean(.data$def_dropback_pct)) / 2,
          league_mean_pass = sum(.data$off_pass_epa * .data$off_pass_epa_n + .data$def_pass_epa * .data$def_pass_epa_n) / sum(.data$off_pass_epa_n + .data$def_pass_epa_n),
          league_mean_rush = sum(.data$off_rush_epa * .data$off_rush_epa_n + .data$def_rush_epa * .data$def_rush_epa_n) / sum(.data$off_rush_epa_n + .data$def_rush_epa_n),
          .groups = "drop"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$season) %>%
        dplyr::mutate(
          league_mean_total = dplyr::lag(cummean(.data$league_mean_total)),
          league_mean_dropback_pct = dplyr::lag(cummean(.data$league_mean_dropback_pct)),
          league_mean_pass = dplyr::lag(cummean(.data$league_mean_pass)),
          league_mean_rush = dplyr::lag(cummean(.data$league_mean_rush))
        ) %>%
        dplyr::ungroup(),
      by = c("season", "week")
    ) %>%
    dplyr::ungroup()
  
  # Adjust EPA for opponent
  pbp <- pbp %>%
    dplyr::mutate(
      # Total off/def adjustments
      off_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_total), .data$league_mean_total - .data$opp_def_epa, 0),
      def_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_total), .data$league_mean_total - .data$opp_off_epa, 0),
      adjusted_off_epa = .data$off_epa + .data$off_adjustment_factor,
      adjusted_def_epa = .data$def_epa + .data$def_adjustment_factor,
      # Dropback pct off/def adjustments
      off_dropback_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_dropback_pct), .data$league_mean_dropback_pct - .data$opp_def_dropback_pct, 0),
      def_dropback_adjustment_factor = dplyr::if_else(!is.na(league_mean_dropback_pct), .data$league_mean_dropback_pct - .data$opp_off_dropback_pct, 0),
      adjusted_off_dropback_pct = .data$off_dropback_pct + .data$off_dropback_adjustment_factor,
      adjusted_def_dropback_pct = .data$def_dropback_pct + .data$def_dropback_adjustment_factor,
      # Pass off/def adjustments
      off_pass_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_pass), .data$league_mean_pass - .data$opp_def_pass_epa, 0),
      def_pass_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_pass), .data$league_mean_pass - .data$opp_off_pass_epa, 0),
      adjusted_off_pass_epa = .data$off_pass_epa + .data$off_pass_adjustment_factor,
      adjusted_def_pass_epa = .data$def_pass_epa + .data$def_pass_adjustment_factor,
      # Rush off/def adjustments
      off_rush_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_rush), .data$league_mean_rush - .data$opp_def_rush_epa, 0),
      def_rush_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_rush), .data$league_mean_rush - .data$opp_off_rush_epa, 0),
      adjusted_off_rush_epa = .data$off_rush_epa + .data$off_rush_adjustment_factor,
      adjusted_def_rush_epa = .data$def_rush_epa + .data$def_rush_adjustment_factor
    )
  
  # Group and calculate rolling average of EPA metrics
  pbp <- pbp %>%
    dplyr::group_by(.data$posteam) %>%
    dplyr::arrange(.data$season, .data$week) %>%
    dplyr::mutate(
      window = ifelse(.data$game_number <= 10, team_n, .data$game_number),
      ### Current metrics
      # Total off/def epa
      off_epa_curr = wt_mov_avg_local(var = .data$off_epa, weight = .data$off_epa_n, window = .data$window, type = epa_off, moving = move),
      def_epa_curr = wt_mov_avg_local(var = .data$def_epa, weight = .data$def_epa_n, window = .data$window, type = epa_def, moving = move),
      adjusted_off_epa_curr = wt_mov_avg_local(var = .data$adjusted_off_epa, weight = .data$off_epa_n, window = .data$window, type = epa_off, moving = move),
      adjusted_def_epa_curr = wt_mov_avg_local(var = .data$adjusted_def_epa, weight = .data$def_epa_n, window = .data$window, type = epa_def, moving = move),
      # Pass off/def epa
      off_pass_epa_curr = wt_mov_avg_local(var = .data$off_pass_epa, weight = .data$off_pass_epa_n, window = .data$window, type = epa_pass_o, moving = move),
      def_pass_epa_curr = wt_mov_avg_local(var = .data$def_pass_epa, weight = .data$def_pass_epa_n, window = .data$window, type = epa_pass_d, moving = move),
      adjusted_off_pass_epa_curr = wt_mov_avg_local(var = .data$adjusted_off_pass_epa, weight = .data$off_pass_epa_n, window = .data$window, type = epa_pass_o, moving = move),
      adjusted_def_pass_epa_curr = wt_mov_avg_local(var = .data$adjusted_def_pass_epa, weight = .data$def_pass_epa_n, window = .data$window, type = epa_pass_d, moving = move),
      # Rush off/def epa
      off_rush_epa_curr = wt_mov_avg_local(var = .data$off_rush_epa, weight = .data$off_rush_epa_n, window = .data$window, type = epa_rush_o, moving = move),
      def_rush_epa_curr = wt_mov_avg_local(var = .data$def_rush_epa, weight = .data$def_rush_epa_n, window = .data$window, type = epa_rush_d, moving = move),
      adjusted_off_rush_epa_curr = wt_mov_avg_local(var = .data$adjusted_off_rush_epa, weight = .data$off_rush_epa_n, window = .data$window, type = epa_rush_o, moving = move),
      adjusted_def_rush_epa_curr = wt_mov_avg_local(var = .data$adjusted_def_rush_epa, weight = .data$def_rush_epa_n, window = .data$window, type = epa_rush_d, moving = move),
      # Dropback pct
      off_dropback_pct_curr = wt_mov_avg_local(var = .data$off_dropback_pct, weight = 1, window = .data$window, type = epa_drop, moving = move),
      def_dropback_pct_curr = wt_mov_avg_local(var = .data$def_dropback_pct, weight = 1, window = .data$window, type = epa_drop, moving = move),
      adjusted_off_dropback_pct_curr = wt_mov_avg_local(var = .data$adjusted_off_dropback_pct, weight = 1, window = .data$window, type = epa_drop, moving = move),
      adjusted_def_dropback_pct_curr = wt_mov_avg_local(var = .data$adjusted_def_dropback_pct, weight = 1, window = .data$window, type = epa_drop, moving = move),
      ### Lagged metrics
      dplyr::across(c(.data$off_epa_curr:.data$adjusted_def_dropback_pct_curr),
                    ~ dplyr::lag(.),
                    .names = "{.col}_{.fn}"
      )
    ) %>%
    dplyr::select(-c(
      .data$off_epa, .data$def_epa, .data$adjusted_off_epa, .data$adjusted_def_epa,
      .data$off_pass_epa, .data$def_pass_epa,
      .data$adjusted_off_pass_epa, .data$adjusted_def_pass_epa,
      .data$off_rush_epa, .data$def_rush_epa,
      .data$adjusted_off_rush_epa, .data$adjusted_def_rush_epa,
      .data$off_dropback_pct, .data$def_dropback_pct,
      .data$adjusted_off_dropback_pct, .data$adjusted_def_dropback_pct
    )) %>%
    dplyr::rename_with(.cols = ends_with("_1"), ~ str_remove(., "_curr_1")) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$game_id, .data$season, .data$week, .data$posteam,
      .data$off_epa, .data$def_epa, .data$adjusted_off_epa, .data$adjusted_def_epa,
      .data$off_dropback_pct, .data$def_dropback_pct, .data$adjusted_off_dropback_pct,
      .data$adjusted_def_dropback_pct, .data$off_pass_epa, .data$def_pass_epa,
      .data$adjusted_off_pass_epa, .data$adjusted_def_pass_epa, .data$off_rush_epa,
      .data$def_rush_epa, .data$adjusted_off_rush_epa, .data$adjusted_def_rush_epa,
      ends_with("_curr")
    )
  
  ### Get schedule and game outcomes from Lee Sharpe
  # Double games (one row per team rather than one row per game)
  weekly_outcomes <- read_csv("Documents/nfl/data/game_results.csv") 
  
  weekly_outcomes <- weekly_outcomes %>%
    dplyr::transmute(.data$season, .data$week,
                     game_date = .data$gameday, .data$game_id,
                     .data$home_team, .data$away_team, .data$home_score, .data$away_score,
                     team = .data$away_team,
                     opponent = .data$home_team,
                     points_for = .data$away_score,
                     points_against = .data$home_score,
                     point_differential = -.data$result
    ) %>%
    dplyr::bind_rows(., weekly_outcomes %>%
                       dplyr::transmute(.data$season, .data$week,
                                        game_date = .data$gameday, .data$game_id,
                                        .data$home_team, .data$away_team, .data$home_score, .data$away_score,
                                        team = .data$home_team,
                                        opponent = .data$away_team,
                                        points_for = .data$home_score,
                                        points_against = .data$away_score,
                                        point_differential = .data$result
                       )) %>%
    dplyr::mutate(
      win = dplyr::if_else(.data$point_differential > 0, 1, 0),
      winner = dplyr::if_else(.data$point_differential > 0, .data$team, .data$opponent),
      loser = dplyr::if_else(.data$point_differential < 0, .data$team, .data$opponent)
    ) %>%
    dplyr::arrange(.data$season, .data$week) %>%
    dplyr::group_by(.data$season, .data$team) %>%
    dplyr::mutate(game_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(c(.data$game_number, .data$team, .data$opponent, .data$winner, .data$loser), .after = .data$game_id)
  
  # Join back opponent game outcome metrics
  weekly_outcomes <- weekly_outcomes %>%
    dplyr::left_join(weekly_outcomes %>%
                       dplyr::select(.data$game_id, .data$season, .data$week, .data$game_number, .data$team, .data$opponent, .data$points_for, .data$points_against) %>%
                       dplyr::rename(
                         opp_points_for = points_for,
                         opp_points_against = points_against,
                         opp_game_number = game_number
                       ) %>%
                       dplyr::group_by(.data$team) %>%
                       dplyr::arrange(.data$season, .data$week) %>%
                       dplyr::mutate(
                         window = dplyr::if_else(.data$opp_game_number <= 10, team_n, .data$opp_game_number),
                         opp_points_for = dplyr::lag(wt_mov_avg_local(var = .data$opp_points_for, weight = 1, window = .data$window, type = pt_diff_type, moving = move)),
                         opp_points_against = dplyr::lag(wt_mov_avg_local(var = .data$opp_points_against, weight = 1, window = .data$window, type = pt_diff_type, moving = move))
                       ) %>%
                       dplyr::select(-c(.data$season, .data$week)),
                     by = c("game_id", "team" = "opponent", "opponent" = "team")
    )
  
  # Join league average point scoring, differentials for opponent adjustments
  weekly_outcomes <- weekly_outcomes %>%
    dplyr::left_join(
      dplyr::filter(weekly_outcomes, .data$team == .data$home_team) %>%
        dplyr::group_by(.data$season, .data$week) %>%
        dplyr::summarise(
          league_mean_pts = mean(.data$points_for),
          .groups = "drop"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$season) %>%
        dplyr::mutate(league_mean_pts = dplyr::lag(cummean(.data$league_mean_pts))),
      by = c("season", "week")
    ) %>%
    # Adjust points for
    dplyr::mutate(
      off_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_pts) & !is.na(.data$opp_points_against), .data$league_mean_pts - .data$opp_points_against, 0),
      def_adjustment_factor = dplyr::if_else(!is.na(.data$league_mean_pts) & !is.na(.data$opp_points_for), .data$league_mean_pts - .data$opp_points_for, 0),
      adjusted_points_for = .data$points_for + .data$off_adjustment_factor,
      adjusted_points_against = .data$points_against + .data$def_adjustment_factor,
      adjusted_point_differential = .data$adjusted_points_for - .data$adjusted_points_against
    )
  
  # Group and calculate rolling average of point differential metrics
  weekly_outcomes <- weekly_outcomes %>%
    dplyr::group_by(.data$team) %>%
    dplyr::arrange(.data$season, .data$week) %>%
    dplyr::mutate(
      window = dplyr::if_else(.data$game_number <= 10, team_n, .data$game_number),
      ### Current metrics
      adjusted_points_for_curr = wt_mov_avg_local(var = .data$adjusted_points_for, weight = 1, window = .data$window, type = pt_diff_type, moving = move),
      adjusted_points_against_curr = wt_mov_avg_local(var = .data$adjusted_points_against, weight = 1, window = .data$window, type = pt_diff_type, moving = move),
      point_differential_curr = wt_mov_avg_local(var = .data$point_differential, weight = 1, window = .data$window, type = pt_diff_type, moving = move),
      adjusted_point_differential_curr = wt_mov_avg_local(var = .data$adjusted_point_differential, weight = 1, window = .data$window, type = pt_diff_type, moving = move),
      ### Lagged metrics
      across(c(.data$adjusted_points_for_curr:.data$adjusted_point_differential_curr),
             ~ dplyr::lag(.),
             .names = "{.col}_{.fn}"
      )
    ) %>%
    dplyr::select(-c(
      .data$point_differential, .data$adjusted_point_differential,
      .data$adjusted_points_for, .data$adjusted_points_against
    )) %>%
    dplyr::rename_with(.cols = ends_with("_1"), ~ str_remove(., "_curr_1")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$league_mean_pts, .data$off_adjustment_factor, .data$def_adjustment_factor))
  
  ### Create Model Dataset
  model_dataset <- weekly_outcomes %>%
    # Add opponent box score statistics
    dplyr::left_join(weekly_outcomes %>%
                       dplyr::select(
                         .data$game_id, .data$team, .data$adjusted_points_for, .data$adjusted_points_against,
                         .data$point_differential, .data$adjusted_point_differential
                       ) %>%
                       dplyr::rename(
                         opp_point_differential = point_differential,
                         opp_adjusted_points_for = adjusted_points_for,
                         opp_adjusted_points_against = adjusted_points_against,
                         opp_adjusted_point_differential = adjusted_point_differential
                       ),
                     by = c("game_id", "opponent" = "team")
    ) %>%
    # Add EPA statistics
    dplyr::left_join(pbp, by = c("game_id", "season", "week", "home_team" = "posteam")) %>%
    dplyr::left_join(pbp %>%
                       dplyr::rename(
                         # Total off/def EPA
                         opp_off_epa = off_epa,
                         opp_def_epa = def_epa,
                         opp_adjusted_off_epa = adjusted_off_epa,
                         opp_adjusted_def_epa = adjusted_def_epa,
                         # Dropback off/def pct
                         opp_off_dropback_pct = off_dropback_pct,
                         opp_def_dropback_pct = def_dropback_pct,
                         opp_adjusted_off_dropback_pct = adjusted_off_dropback_pct,
                         opp_adjusted_def_dropback_pct = adjusted_def_dropback_pct,
                         # Pass off/def EPA
                         opp_off_pass_epa = off_pass_epa,
                         opp_def_pass_epa = def_pass_epa,
                         opp_adjusted_off_pass_epa = adjusted_off_pass_epa,
                         opp_adjusted_def_pass_epa = adjusted_def_pass_epa,
                         # Rush off/def EPA
                         opp_off_rush_epa = off_rush_epa,
                         opp_def_rush_epa = def_rush_epa,
                         opp_adjusted_off_rush_epa = adjusted_off_rush_epa,
                         opp_adjusted_def_rush_epa = adjusted_def_rush_epa
                       ),
                     by = c("game_id", "season", "week", "away_team" = "posteam")
    ) %>%
    dplyr::filter(.data$home_team == .data$team) %>%
    # Add home margin
    dplyr::mutate(home_margin = .data$home_score - .data$away_score) %>%
    # Filter NAs
    dplyr::filter(!is.na(.data$off_epa)) %>%
    # Add numeric ID
    dplyr::mutate(numeric_id = row_number()) %>%
    dplyr::rename(gameday = game_date)
  return(model_dataset)
}


