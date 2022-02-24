

library(tidyverse)

nfl_games <- nflfastR::fast_scraper_schedules(2021) %>%
  dplyr::mutate(
    gameday = lubridate::as_date(gameday)
  )

FiveThirtyEight_Predictions <- read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv") %>%
  dplyr::filter(season == 2021) %>%
  dplyr::mutate(
    team1 = gsub("WSH", "WAS", team1),
    team2 = gsub("WSH", "WAS", team2),
    team1 = gsub("LAR", "LA", team1),
    team2 = gsub("LAR", "LA", team2)
  ) %>%
  dplyr::select(date, season, team1, team2, elo_prob1) %>%
  dplyr::rename(fivethirtyeight_home_wp = elo_prob1)


nfl_games <- nfl_games %>%
  left_join(
    FiveThirtyEight_Predictions,
    by = c("home_team" = "team1", "away_team" = "team2", "gameday" = "date", "season")
  )


### ESPN
### 



get_game_ids <- function(season, season_type = c("preseason", "regular", "postseason")) {

  # https://www.opensourcefootball.com/posts/2020-08-29-adding-espn-and-538-game-predictions-to-nflfastr-data/

  current_year <- as.double(substr(Sys.Date(), 1, 4))
  espn_game_ids <- data.frame()

  if (!season_type %in% c("preseason", "regular", "postseason", "all")) {
    stop("Please choose season_type of 'regular',  'playoffs', 'postseason', or 'all'")
  }

  if (!dplyr::between(as.numeric(season), 2002, current_year)) {
    stop(paste("Please choose season between 2002 and", current_year))
  }

  if (lubridate::month(Sys.Date()) < 12 & lubridate::month(Sys.Date()) > 2 & 
      season_type == "postseason" & current_year == season | season_type == "postseason" & 
      lubridate::month(Sys.Date()) <= 2 & current_year == season) {
    stop(paste("Unfortunately, the NFL Playoff Games have not been determined yet"))
  }

  message(
    dplyr::if_else(
      season_type == "regular",
      glue::glue("Scraping from {season} {season_type} season!"),
      glue::glue("Scraping from {season} {season_type}!")
    )
  )

  season_type <- ifelse(season_type == "preseason", "1", season_type)
  season_type <- ifelse(season_type == "regular", "2", season_type)
  season_type <- ifelse(season_type == "postseason", "3", season_type)

  weeks <- ifelse(season_type == "2", 17, 5)

  espn_game_ids <- purrr::map_df(1:weeks, function(week) {
    url <- glue::glue("https://www.espn.com/nfl/schedule/_/week/{week}/year/{season}/seasontype/{season_type}/2")
                  

    webpage <- xml2::read_html(url)

    links <- webpage %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")

    espn_gameid <- links %>%
      as_tibble() %>%
      dplyr::filter(str_detect(value, "gameId") == TRUE) %>%
      dplyr::pull(value) %>%
      stringr::str_remove(., "/nfl/game/_/gameId/")

    bye_teams <- webpage %>%
      rvest::html_nodes(".odd.byeweek") %>%
      rvest::html_nodes("abbr") %>%
      rvest::html_text()

    home_team <- webpage %>%
      rvest::html_nodes(".home-wrapper") %>%
      rvest::html_nodes("abbr") %>%
      rvest::html_text()

    away_team <- webpage %>%
      rvest::html_nodes("abbr") %>%
      rvest::html_text()
    away_team <- away_team[!away_team %in% home_team]
    away_team <- away_team[!away_team %in% bye_teams]

    placeholder <- data.frame(
      home_team,
      away_team,
      espn_gameid
    ) %>%
      dplyr::mutate(
        season_type = season_type,
        season = season,
        week = ifelse(season_type == 3, 17 + week, week)
      )

    espn_game_ids <- dplyr::bind_rows(espn_game_ids, placeholder)
    return(espn_game_ids)
  })

  ### Fix Several Names for Compatibility with nflfastR Data game_ids
  espn_game_ids <- espn_game_ids %>%
    dplyr::mutate(
      home_team = gsub("WSH", "WAS", home_team),
      away_team = gsub("WSH", "WAS", away_team),
      home_team = gsub("LAR", "LA", home_team),
      away_team = gsub("LAR", "LA", away_team)
    ) %>%
    # Add nflfastR game_ids
    dplyr::mutate(
      week = ifelse(week == 22, week - 1, week),
      alt_gameid = paste0(season, "_", ifelse(week >= 10, paste0(week), paste0(0, week)), "_", away_team, "_", home_team)
    )

  return(espn_game_ids)
}

# Get Game IDs

ESPN_Games <- purrr::map_df(2021, function(x) {
  get_game_ids(x, season_type = "regular")
})



# Pull Pregame Predictions

ESPN_Game_Predictions <- purrr::map_df(ESPN_Games$espn_gameid, function(espn_game_id) {
  pregame_predictions <- data.frame(espn_gameid = espn_game_id)

  # Pull the JSon
  game_json <- httr::GET(url = glue::glue(
    "http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={espn_game_id}"
  )) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)


  # Pull the game data from the ID dataframe
  if ("gameProjection" %in% names(game_json[["predictor"]][["homeTeam"]]) == TRUE) {
    pregame_predictions <- pregame_predictions %>%
      dplyr::mutate(
        espn_home_wp = as.numeric(game_json[["predictor"]][["homeTeam"]][["gameProjection"]]) / 100
      )
    message(
      paste("Pulling predictions for", pregame_predictions$alt_gameid)
    )
  }


  # Grab and convert the Moneylines from Oddsmakers
  if ("pickcenter" %in% names(game_json) == TRUE &
    "provider.name" %in% names(game_json[["pickcenter"]]) == TRUE &
    "homeTeamOdds.moneyLine" %in% names(game_json[["pickcenter"]]) == TRUE
  ) {
    vegas_odds <- data.frame(
      providers = game_json[["pickcenter"]][["provider.name"]],
      odds = ifelse(game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]] > 0, 100 / (game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]] + 100), game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]] / (game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]] - 100))
    ) %>%
      tidyr::pivot_wider(names_from = providers, values_from = odds)

    pregame_predictions <- cbind(
      pregame_predictions, vegas_odds
    )
  }

  return(pregame_predictions)
})


# Merge ESPN Data together
ESPN_Games <- ESPN_Games %>%
  left_join(
    ESPN_Game_Predictions,
    by = c("espn_gameid" = "espn_gameid")
  )

# Merge back to main data
nfl_games <- nfl_games %>%
  left_join(
    ESPN_Games %>% select(alt_gameid, espn_home_wp, Caesars = "Caesars Sportsbook (New Jersey)", numberfire, teamrankings, consensus),
    by = c("game_id" = "alt_gameid")
  )

# Now that it’s all together, let’s take a look at the accuracy of ESPN’s, 538’s, 
# Numberfire’s predictions over 2021.

### Do some data wrangling first
nfl_games <- nfl_games %>%
  mutate(
    home_win = ifelse(home_score > away_score, 1, 0),
    correct_espn = ifelse(ifelse(espn_home_wp > .5, 1, 0) == home_win, 1, 0),
    correct_numberfire = ifelse(ifelse(numberfire > .5, 1, 0) == home_win, 1, 0),
    correct_fivethirtyeight = ifelse(ifelse(fivethirtyeight_home_wp > .5, 1, 0) == home_win, 1, 0)
  )

Accuracy_Dataset <- nfl_games %>%
  # Filter out Playoff Games
  filter(game_type == "REG") %>%
  # Pivot Longer to allow group_by and summarize
  pivot_longer(
    cols = starts_with("correct_"),
    names_to = "predictor",
    names_prefix = "correct_",
    values_to = "correct"
  ) %>%
  # Group and Summarize
  group_by(predictor) %>%
  summarise(
    games = sum(!is.na(correct)),
    games_correct = sum(correct, na.rm = TRUE),
    percent_correct = round(mean(correct, na.rm = TRUE), 3)
  )

### Merge Over Brier Scores
Accuracy_Dataset <- Accuracy_Dataset %>%
  left_join(data.frame(
    predictor = c("espn", "fivethirtyeight", "numberfire"),
    brier_score = c(
      DescTools::BrierScore(dplyr::filter(nfl_games, !is.na(espn_home_wp)) %>% pull(home_win), dplyr::filter(nfl_games, !is.na(espn_home_wp)) %>% pull(espn_home_wp)),
      DescTools::BrierScore(dplyr::filter(nfl_games, !is.na(home_win)) %>% pull(home_win), dplyr::filter(nfl_games, !is.na(fivethirtyeight_home_wp)) %>% pull(fivethirtyeight_home_wp)),
      DescTools::BrierScore(dplyr::filter(nfl_games, !is.na(numberfire)) %>% pull(home_win), dplyr::filter(nfl_games, !is.na(numberfire)) %>% pull(numberfire))
    )
  ),
  by = "predictor"
  ) %>%
  mutate(brier_score = round(brier_score, 3))



### Create a fancy reactable table
library(reactable)

Accuracy_Dataset %>%
  arrange(-percent_correct) %>%
  reactable(
    compact = TRUE,
    borderless = FALSE,
    striped = FALSE,
    fullWidth = TRUE,
    defaultColDef = colDef(
      align = "center",
      minWidth = 100),
    theme = reactableTheme(
      headerStyle = list(
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
        borderColor = "#555")
    ),
    columns = list(
      predictor = colDef(name = "Predictor",
                         align = "left",
                         minWidth = 110),
      games = colDef(name = "Predictions"),
      games_correct = colDef(name = "Correct Predictions"),
      percent_correct = colDef(name = "% Correct",
                               class = "border-left",
                               style = function(value) {
                                 normalized <- (value - min(Accuracy_Dataset$percent_correct)) / (max(Accuracy_Dataset$percent_correct) - min(Accuracy_Dataset$percent_correct))
                                 color <- rgb(colorRamp(c("#0089BA","#00B0A7"))(normalized), maxColorValue = 255)
                                 list(background = color)}
      ),
      brier_score = colDef(name = "Brier Score", 
                           style = function(value) {
                             normalized <- (value - min(Accuracy_Dataset$brier_score)) / (max(Accuracy_Dataset$brier_score) - min(Accuracy_Dataset$brier_score))
                             color <- rgb(colorRamp(c("#0089BA", 	"#00B0A7"))(normalized), maxColorValue = 255)
                             list(background = color)
                           }
      )
    )
  )