
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




load_ngs <- function(
  stat_type = c("passing", "receiving", "rushing"), year = c(2021),
  weekly = TRUE, regular_season = TRUE, join_roster = FALSE, pbp_data = pbp_data
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
  
  # if true, join the ngs data to pbp
  if (isTRUE(join_roster)) {
    
    ngs <- pbp_data %>%
      dplyr::left_join(ngs, by = c("jersey_number" = "player_jersey_number", "posteam" = "team_abbr", "season", "name" = "player_short_name"))
    
    
  }
  
  
  return(ngs)
}

