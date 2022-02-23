
game_results <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv") %>%
  dplyr::mutate(dplyr::across(
    c("home_team", "away_team"),
    ~ stringr::str_replace_all(., c(
      "JAC" = "JAX",
      "STL" = "LA",
      "SL" = "LA",
      "ARZ" = "ARI",
      "BLT" = "BAL",
      "CLV" = "CLE",
      "HST" = "HOU",
      "SD" = "LAC",
      "OAK" = "LV"
    ))
  ))

write_csv(game_results, "~/Documents/nfl/data/game_results.csv")