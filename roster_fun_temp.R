
library(tidyverse)

# get pbp data for the years. Can take multiple values

get_pbp_data <- function(seasons = c(2021)){
  
  pbp <- purrr::map_df(c(seasons), function(x) {
    readr::read_rds(
      glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
    )
  })
  
}

# using pbp data it will add roster data for the positions of interest
# will take longer as you increase the years

join_roster_data <- function(pbp, season = c(2021), pos = c('RB', 'TE')){
  
  roster <- purrr::map_df(c(season), function(x) {
    readr::read_rds(
      glue::glue("~/Documents/nfl/data/roster/roster_{x}.rds")
    )
  })
  
  pbp %>%
    dplyr::filter(
      .data$season_type == "REG", 
      .data$down <= 4,
      !is.na(.data$rusher_id)
    ) %>%
    dplyr::left_join(
      dplyr::filter(roster, .data$position %in% pos), 
      by = c("receiver_id" = "gsis_id")) 
  
}



# load your ngs data

load_ngs <- function(
  stat_type = c("passing", "receiving", "rushing"), year = c(2021),
  weekly = TRUE, regular_season = TRUE
){
  
  stat_type <- rlang::arg_match0(stat_type, c("passing", "receiving", "rushing"))
  
  ngs <- read_rds(glue::glue("~/Documents/nfl/data/ngs/ngs_{stat_type}.rds"))
  
  if (isTRUE(weekly)) {
    ngs <- dplyr::filter(ngs, .data$week >= 1, .data$season == year)
  }
  if (isFALSE(weekly)) {
    ngs <- dplyr::filter(ngs, .data$week == 0, .data$season == year)
  }
  
  # if true, drop postseason games
  if (isTRUE(regular_season)) {
    ngs <- dplyr::filter(ngs, .data$season_type == 'REG')
  }
  
  return(ngs)
}


ngs_read <- function(year = c(2021), stat_type = c(0)){
  
  ngs <- read_rds("~/Documents/nfl/data/ngs/ngs_rushing.rds") %>%
    dplyr::filter(
      .data$season == year,
      .data$week == {{season}},
      .data$season_type == 'REG'
    ) %>%
    dplyr::select(
      .data$season, .data$week, player_name = .data$player_display_name, .data$player_short_name, 
      position = .data$player_position, team = .data$team_abbr, .data$player_gsis_id
    )
  
}


join_roster_data_ngs <- function(pbp, year = c(2021), stat_type = "season"){
  
  "season" = 0
  "weekly" = 1:18
  
  ngs <- read_rds("~/Documents/nfl/data/ngs/ngs_rushing.rds") %>%
    dplyr::filter(
      .data$season == year,
      .data$week == stat_type
      .data$season_type == 'REG'
      ) %>%
    dplyr::select(
      .data$season, .data$week, player_name = .data$player_display_name, .data$player_short_name, 
      position = .data$player_position, team = .data$team_abbr, .data$player_gsis_id
    )
  
  pbp %>%
    dplyr::filter(
      .data$season_type == "REG", 
      .data$down <= 4
    ) %>%
    dplyr::left_join(
      dplyr::filter(roster, .data$position %in% pos), 
      by = c("receiver_id" = "gsis_id")) 
  
}