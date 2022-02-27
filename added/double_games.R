# double games
## takes input where each game has one row with teams as `away_team` and `home_team`
## returns with each game having two rows with teams listed as `team` and `opp`
double_games <- function(g)
{
  g1 <- g %>% 
    rename(team=away_team,team_score=away_score,
           opp=home_team,opp_score=home_score,
           team_rest=away_rest,opp_rest=home_rest,
           team_moneyline=away_moneyline,opp_moneyline=home_moneyline,
           team_spread_odds=away_spread_odds,opp_spread_odds=home_spread_odds,
           team_coach=away_coach,opp_coach=home_coach) %>% 
    mutate(location=ifelse(location == "Home","Away",location),
           result=-1*result,spread_line=-1*spread_line)
  g2 <- g %>% 
    rename(team=home_team,team_score=home_score,
           opp=away_team,opp_score=away_score,
           team_rest=home_rest,opp_rest=away_rest,
           team_moneyline=home_moneyline,opp_moneyline=away_moneyline,
           team_spread_odds=home_spread_odds,opp_spread_odds=away_spread_odds,           
           team_coach=home_coach,opp_coach=away_coach)
  g <- bind_rows(g1,g2) %>% 
    arrange(gameday,gametime,old_game_id,location)
  return(g)
}


# smaller version
double_games2 <- function(g) {
  g1 <- g %>%
    rename(
      team = away_team, 
      team_score = away_score,
      opp = home_team, 
      opp_score = home_score,
      team_name = away_team.y, 
      team_conf = away_conf,
      team_div = away_div
    ) %>%
    mutate(
      location = ifelse(location == "Home", "Away", location),
      result = -1 * result, spread_line = -1 * spread_line
    )
  g2 <- g %>%
    rename(
      team = home_team, 
      team_score = home_score,
      opp = away_team, 
      opp_score = away_score,
      team_name = home_team.y, 
      team_conf = home_conf,
      team_div = home_div
    )
  g <- bind_rows(g1, g2) %>%
    arrange(game_date, game_id, location)
  return(g)
}