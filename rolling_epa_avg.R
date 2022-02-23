

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

