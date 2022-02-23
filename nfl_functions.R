

#based on https://dtai.cs.kuleuven.be/events/MLSA18/papers/pelechrinis_mlsa18.pdf
# and https://www.opensourcefootball.com/posts/2020-08-20-adjusting-epa-for-strenght-of-opponent/

adjust_epa <- function(pbp){
  
  pbp <- pbp %>%
    dplyr::filter(
      !is.na(.data$epa),
      !is.na(.data$ep),
      !is.na(.data$posteam), 
      .data$play_type %in% c("pass", "run")
    ) %>%
    dplyr::group_by(.data$game_id, .data$season, .data$week, .data$posteam, .data$home_team) %>%
    dplyr::summarise(
      off_epa = mean(.data$epa),
    ) %>%
    dplyr::left_join(
      filter(pbp,
             !is.na(.data$epa),
             !is.na(.data$ep), 
             !is.na(.data$posteam),
             .data$play_type %in% c("pass", "run")
      ) %>%
        dplyr::group_by(.data$game_id, .data$season, .data$week, .data$defteam, .data$away_team) %>%
        dplyr::summarise(def_epa = mean(.data$epa)),
      by = c("game_id", "posteam" = "defteam", "season", "week"),
      all.x = TRUE) %>%
    dplyr::mutate(opponent = if_else(.data$posteam == .data$home_team, .data$away_team, .data$home_team)) %>%
    dplyr::select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa)
  
  #adjusting a team’s epa/play based on the strength of the opponent they are up against
  
  #reframe each team’s epa/play as a team’s weekly opponent
  #convert each statistic into a moving average of the last ten games and lag that statistic by one week
  
  # Construct opponent dataset and lag the moving average of their last ten games.
  opponent_data <- pbp %>%
    dplyr::select(-opponent) %>%
    dplyr::rename(
      opp_off_epa = off_epa,
      opp_def_epa = def_epa
    ) %>%
    dplyr::group_by(.data$posteam) %>%
    dplyr::arrange(season, week) %>%
    dplyr::mutate(
      opp_def_epa = pracma::movavg(.data$opp_def_epa, n = 10, type = "s"),
      opp_def_epa = dplyr::lag(.data$opp_def_epa),
      opp_off_epa = pracma::movavg(.data$opp_off_epa, n = 10, type = "s"),
      opp_off_epa = dplyr::lag(.data$opp_off_epa)
    )
  
  # Merge opponent data back in with the weekly epa data
  pbp <- pbp %>%
    left_join(
      opponent_data,
      by = c("game_id", "season", "week", "home_team", "away_team", "opponent" = "posteam"),
      all.x = TRUE
    )
  
  #We need to know how strong the opponent is relative to the average team in the league.
  
  pbp <- pbp %>%
    dplyr::left_join(
      dplyr::filter(pbp, .data$posteam == .data$home_team) %>%
        dplyr::group_by(.data$season, .data$week) %>%
        dplyr::summarise(league_mean = mean(.data$off_epa + .data$def_epa)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$season) %>%
        dplyr::mutate(
          # We lag because we need to know the league mean up to that point in the season
          league_mean = lag(pracma::movavg(.data$league_mean, n = 10, type = "s"), ) ),
      by = c("season", "week"),
      all.x = TRUE)
  
  pbp <- pbp %>%
    dplyr::mutate(
      off_adjustment_factor = if_else(!is.na(.data$league_mean), .data$league_mean - .data$opp_def_epa, 0),
      def_adjustment_factor = if_else(!is.na(.data$league_mean), .data$league_mean - .data$opp_off_epa, 0),
      adjusted_off_epa = .data$off_epa + .data$off_adjustment_factor,
      adjusted_def_epa = .data$def_epa + .data$def_adjustment_factor,
    ) %>%
    dplyr::group_by(.data$posteam, .data$season) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    dplyr::select(posteam, season, off_epa, def_epa, adjusted_off_epa, adjusted_def_epa) %>%
    dplyr::filter(.data$season == 2021) %>%
    dplyr::ungroup()
  
  
  
  return(pbp)
}

pls1 <- adjust_epa(pbp) 
