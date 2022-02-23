

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



# Function to compute predictiveness
double_for_accuracy_check <- function(dataset, type) {
  # Double games (one row per team rather than one row per game)
  g1 <- dataset %>%
    dplyr::transmute(.data$gameday, .data$game_id, .data$season, .data$week,
      team = .data$away_team,
      opponent = .data$home_team,
      win = dplyr::if_else(.data$win == 1, 0, 1),
      margin = -.data$home_margin,
      .data$opp_adjusted_point_differential,
      .data$opp_adjusted_off_epa,
      .data$opp_adjusted_def_epa,
      .data$opp_adjusted_off_pass_epa,
      .data$opp_adjusted_def_pass_epa,
      .data$opp_adjusted_off_rush_epa,
      .data$opp_adjusted_def_rush_epa,
      .data$opp_adjusted_off_dropback_pct,
      .data$opp_adjusted_def_dropback_pct,
      .data$opp_point_differential,
      .data$opp_off_epa,
      .data$opp_def_epa,
      .data$opp_off_pass_epa,
      .data$opp_def_pass_epa,
      .data$opp_off_rush_epa,
      .data$opp_def_rush_epa,
      .data$opp_off_dropback_pct,
      .data$opp_def_dropback_pct
    ) %>%
    dplyr::rename_with(.cols = starts_with("opp_"), ~ stringr::str_remove(., "opp_")) %>%
    dplyr::mutate(location = "Away")

  g2 <- dataset %>%
    dplyr::transmute(.data$gameday, .data$game_id, .data$season, .data$week,
      team = .data$home_team,
      opponent = .data$away_team,
      .data$win,
      margin = .data$home_margin,
      .data$adjusted_point_differential,
      .data$adjusted_off_epa,
      .data$adjusted_def_epa,
      .data$adjusted_off_pass_epa,
      .data$adjusted_def_pass_epa,
      .data$adjusted_off_rush_epa,
      .data$adjusted_def_rush_epa,
      .data$adjusted_off_dropback_pct,
      .data$adjusted_def_dropback_pct,
      .data$point_differential,
      .data$off_epa,
      .data$def_epa,
      .data$off_pass_epa,
      .data$def_pass_epa,
      .data$off_rush_epa,
      .data$def_rush_epa,
      .data$off_dropback_pct,
      .data$def_dropback_pct
    ) %>%
    dplyr::mutate(location = "Home")

  doubled <- dplyr::bind_rows(g1, g2) %>%
    dplyr::arrange(.data$game_id, .data$gameday, .data$season, .data$week) %>%
    dplyr::mutate(
      off_pass_rush_epa = .data$off_dropback_pct * .data$off_pass_epa + (1 - .data$off_dropback_pct) * .data$off_rush_epa,
      def_pass_rush_epa = .data$def_dropback_pct * .data$def_pass_epa + (1 - .data$adjusted_def_dropback_pct) * .data$adjusted_def_rush_epa,
      adjusted_off_pass_rush_epa = .data$adjusted_off_dropback_pct * .data$adjusted_off_pass_epa + (1 - .data$adjusted_off_dropback_pct) * .data$adjusted_off_rush_epa,
      adjusted_def_pass_rush_epa = .data$adjusted_def_dropback_pct * .data$adjusted_def_pass_epa + (1 - .data$adjusted_def_dropback_pct) * .data$adjusted_def_rush_epa
    )

  results <- doubled %>%
    dplyr::filter(season > 1999) %>%
    tidyr::pivot_longer(
      cols = c(-c(.data$gameday:.data$margin, .data$location)),
      names_to = "metric",
      values_to = "value"
    ) %>%
    tidyr::nest(data = c(-.data$metric)) %>%
    dplyr::mutate(
      regression = map(data, ~ glm(win ~ value, data = ., family = "binomial")),
      r_squared = map(regression, fmsb::NagelkerkeR2)
    ) %>%
    tidyr::hoist(r_squared, r.squared = "R2") %>%
    dplyr::arrange(desc(r.squared)) %>%
    dplyr::select(metric, r.squared) %>%
    dplyr::mutate(moving_avg = type)
}


# Function to plot results
plot_results <- function(results) {
  results %>% 
    dplyr::mutate(name = stringr::str_to_title(stringr::str_replace_all(metric, "_", " ")),
                  name = stringr::str_remove(name, "usted"),
                  name = stringr::str_replace_all(name, "Point Differential", "Pt Diff"),
                  name = stringr::str_remove(name, " Epa"),
                  type = dplyr::case_when(
                    stringr::str_detect(metric, "pass_rush") ~ "Complex EPA",
                    stringr::str_detect(metric, "pass") ~ "Pass EPA",
                    stringr::str_detect(metric, "rush") ~ "Rush EPA",
                    stringr::str_detect(metric, "off_epa|def_epa") ~ "Total EPA",
                    stringr::str_detect(metric, "point_diff") ~ "Point Diff",
                    stringr::str_detect(metric, "dropback") ~ "Dropback")) %>%
    dplyr::mutate(type = forcats::fct_relevel(type, "Point Diff", "Total EPA", "Complex EPA", "Pass EPA", "Rush EPA"),
                  name = forcats::fct_reorder(name, r.squared),
                  moving_avg = forcats::fct_reorder(moving_avg, r.squared)) %>% 
    dplyr::filter(type != "Dropback") %>% 
    ggplot(aes(r.squared, name, fill = moving_avg)) +
    geom_col(position = position_dodge(0.9)) +
    geom_text(aes(label = round(r.squared, 4)),
              hjust = 1, size = 2.5, color = "white", fontface = "bold",
              position = position_dodge(0.9)) +
    facet_wrap( ~ type, scales = "free") +
    scale_x_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 28/.pt, hjust = 0),
          plot.subtitle = element_text(face = "italic", size = 24/.pt),
          strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
          strip.text.x = element_text(face = "bold"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 24/.pt),
          axis.title = element_text(face = "bold", size = 26/.pt),
          plot.caption = element_text(face = "italic", size = 20/.pt),
          legend.position = c(0.8, 0.2)) +
    labs(x = expression(bold("Nagelkerke pseudo"~R^2)),
         y = NULL,
         fill = NULL,
         title = "Predictive power of various EPA metrics for the outcome of an NFL game",
         subtitle = "Logistic regression to predict win or loss",
         caption = "Chart: @jacklich10 | Data: @nflfastR | Games from 2000-2020 NFL seasons")
}




weekly_epa_df <- weekly_epa_play(pbp)

# Simple static window
model_dataset <- create_rolling_data(weekly_epa_df,
  move = FALSE, pt_diff_type = "s",
  epa_off = "s", epa_pass_o = "s", epa_rush_o = "s",
  epa_def = "s", epa_pass_d = "s", epa_rush_d = "s",
  epa_drop = "s"
)

results_simple <- double_for_accuracy_check(dataset = model_dataset, type = "Simple")

# Dynamic window
model_dataset <- create_rolling_data(weekly_epa_df,
  move = TRUE, pt_diff_type = "s",
  epa_off = "s", epa_pass_o = "s", epa_rush_o = "s",
  epa_def = "s", epa_pass_d = "s", epa_rush_d = "s",
  epa_drop = "s"
)

results_dynamic <- double_for_accuracy_check(dataset = model_dataset, type = "Dynamic")

results <- dplyr::bind_rows(results_simple, results_dynamic)


# a bit hacky, but it is still fairly tidy
results %>%
  mutate(
    metric = str_replace_all(metric, "[\\s_]+", " ") %>%
      str_to_title() %>%
      str_replace_all(., "Epa", "EPA")
  ) %>%
  ggplot(aes(
    y = fct_reorder(metric, r.squared),
    fill = moving_avg,
    x = r.squared
  )) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      Dynamic = "#E3A082",
      Simple = "#7499BD"
    )
  ) +
  labs(
    x = "Feature",
    y = "R Squared",
    fill = "Type"
  ) +
  becausejustynfun::white_theme()




# Weighted
model_dataset <- create_rolling_data(weekly_epa_df, move = TRUE, pt_diff_type = "w", 
                                     epa_off = "w", epa_pass_o = "w", epa_rush_o = "w", 
                                     epa_def = "w", epa_pass_d = "w", epa_rush_d = "w", 
                                     epa_drop = "w")

results_weighted <- double_for_accuracy_check(dataset = model_dataset, type = "Weighted")

# Runnning
model_dataset <- create_rolling_data(weekly_epa_df, move = TRUE, pt_diff_type = "r", 
                                     epa_off = "r", epa_pass_o = "r", epa_rush_o = "r", 
                                     epa_def = "r", epa_pass_d = "r", epa_rush_d = "r", 
                                     epa_drop = "r")
results_running <- double_for_accuracy_check(dataset = model_dataset, type = "Running")

# Exponential
model_dataset <- create_rolling_data(weekly_epa_df, move = TRUE, pt_diff_type = "e", 
                                     epa_off = "e", epa_pass_o = "e", epa_rush_o = "e", 
                                     epa_def = "e", epa_pass_d = "e", epa_rush_d = "e", 
                                     epa_drop = "e")

results_exp <- double_for_accuracy_check(dataset = model_dataset, type = "Exponential")

results <- dplyr::bind_rows(results_dynamic %>%
  dplyr::mutate(moving_avg = "Simple"), results_weighted, results_running, results_exp)

results %>%
  mutate(
    metric = str_replace_all(metric, "[\\s_]+", " ") %>%
      str_to_title() %>%
      str_replace_all(., "Epa", "EPA")
  ) %>%
  ggplot(aes(
    x = r.squared,
    y = fct_reorder(metric, r.squared),
    fill = moving_avg
  )) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(Exponential = "#66C2A5",
               Running = "#AB98C8",
               Simple = "#E1D83B",
               Weighted = "#B3B3B3")
  ) +
  labs(
    x = "Feature",
    y = "R Squared",
    fill = "Type"
  ) +
  becausejustynfun::white_theme()



# Mixed moving averages
model_dataset <- create_rolling_data(weekly_epa_df, move = T, pt_diff_type = "r", 
                                     epa_off = "e", epa_pass_o = "e", epa_rush_o = "r", 
                                     epa_def = "r", epa_pass_d = "r", epa_rush_d = "r", 
                                     epa_drop = "r")
results_mixed <- double_for_accuracy_check(dataset = model_dataset, type = "Mixed")
results <- dplyr::bind_rows(results_dynamic %>% 
                              dplyr::mutate(moving_avg = "Simple"), 
                            results_weighted, results_running, results_exp, results_mixed)


plot_results(results) +
  scale_fill_brewer(palette = "Dark2", 
                    guide = guide_legend(reverse = T))
  




# Function to display a team's efficiency over time
efficiency_over_time <- function(dataset, current_season, current_week, team_name) {
  # Double games (one row per team rather than one row per game)
  g1 <- dataset %>% 
    dplyr::transmute(gameday, game_id, season, week, 
                     team = away_team,
                     opponent = home_team,
                     adjusted_off_epa = opp_adjusted_off_epa,
                     adjusted_def_epa = opp_adjusted_def_epa) %>% 
    dplyr::mutate(location = "Away")
  
  g2 <- dataset %>% 
    dplyr::transmute(gameday, game_id, season, week, 
                     team = home_team,
                     opponent = away_team,
                     adjusted_off_epa,
                     adjusted_def_epa) %>% 
    dplyr::mutate(location = "Home")
  
  # Bind together and fill in offense/defense efficiency for bye weeks
  doubled <- dplyr::bind_rows(g1, g2) %>% 
    dplyr::arrange(game_id, gameday, season, week) %>% 
    tidyr::complete(season, week, team) %>% 
    dplyr::mutate(season = ifelse(week == 1, season - 1, season),
                  week = ifelse(week == 1, 17, week - 1)) %>% 
    dplyr::group_by(team) %>% 
    tidyr::fill(adjusted_off_epa, adjusted_def_epa, .direction = "updown") %>% 
    dplyr::group_by(season, week) %>% 
    dplyr::mutate(off_rank = rank(-adjusted_off_epa, ties.method = "random"),
                  def_rank = rank(adjusted_def_epa, ties.method = "random")) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(nflfastR::teams_colors_logos %>%
                       select(team_abbr, team_logo_espn), 
                     by = c("team" = "team_abbr")) %>% 
    #dplyr::filter(week <= 17, is.na(gameday)) %>%
    dplyr::filter(season %in% current_season, week <= current_week) %>% 
    tidyr::pivot_longer(cols = ends_with("_rank"), names_to = "side", values_to = "rank") %>% 
    dplyr::mutate(side = ifelse(side == "off_rank", "Offensive Efficiency", "Defensive Efficiency"),
                  side = fct_relevel(side, "Offensive Efficiency", "Defensive Efficiency"))
  
  if (length(current_season) > 1) {
    season <- paste0(current_season[1], "-", current_season[length(current_season)])
  } else {
    season <- current_season
  }
  
  plot <- doubled %>% 
    ggplot(aes(week, rank, color = team)) +
    geom_line(aes(size = ifelse(team == team_name, "A", "B"), alpha = ifelse(team == team_name, 1, 0.2)), 
              show.legend = F) +
    ggimage::geom_image(aes(max(week), rank, image = ifelse(week == max(week[team == team_name]) & team == team_name, team_logo_espn, NA)),
                        asp = 1.618, by = "height", size = 0.15, inherit.aes = F) +
    facet_wrap(~ side, nrow = 1) +
    scale_x_continuous(breaks = seq(0, 17, by = 1)) +
    scale_y_reverse(breaks = seq(1, 32, by = 3)) +
    scale_size_manual(values = c(1.5, 0.75)) +
    theme_bw() +
    theme(aspect.ratio = 9/16,
          plot.title = element_text(face = "bold", size = 28/.pt, hjust = 0),
          plot.subtitle = element_text(face = "italic", size = 24/.pt),
          strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
          strip.text.x = element_text(face = "bold"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 24/.pt),
          axis.title = element_text(face = "bold", size = 26/.pt),
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          plot.caption = element_text(face = "italic", size = 20/.pt)) +
    labs(title = paste0(season, " ", team_name, " Efficiency"),
         subtitle = "EPA/Play adjusted for opponent",
         x = "Week",
         y = "League\nRank",
         caption =  "Chart: @jacklich10 | Data: @nflfastR")
  
  if (length(current_season) > 1) {
    return(plot + facet_wrap(season ~ side))
  } else {
    return(plot + facet_wrap(~ side, nrow = 1))
  }
}


efficiency_over_time(model_dataset, current_season = 2021, current_week = 17, team_name = "PHI")
