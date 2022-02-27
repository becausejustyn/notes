

add_divisions <- function(df){
  
  df <- df %>%
    dplyr::mutate(
      division = dplyr::case_when(
        .data$team %in% c("GB", "MIN", "DET", "CHI") ~ "NFC North",
        .data$team %in% c("ATL", "CAR", "NO", "TB") ~ "NFC South",
        .data$team %in% c("DAL", "NYG", "PHI", "WAS") ~ "NFC East",
        .data$team %in% c("ARI", "LA", "SEA", "SF") ~ "NFC West",
        .data$team %in% c("BAL", "CIN", "CLE", "PIT") ~ "AFC North",
        .data$team %in% c("HOU", "IND", "JAX", "TEN") ~ "AFC South",
        .data$team %in% c("BUF", "MIA", "NE", "NYJ") ~ "AFC East",
        .data$team %in% c("DEN", "KC", "LV", "LAC") ~ "AFC West"
      ),
      conference = dplyr::if_else(.data$division %in% c("AFC North", "AFC South", "AFC East", "AFC West"), "AFC", "NFC"))
  
  return(df)
}