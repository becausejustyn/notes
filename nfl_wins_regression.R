
# https://github.com/nflverse/open-source-football/blob/master/_posts/2020-08-23-exploring-wins-with-nflfastr/regression_code.R

library(tidyverse)
library(ggimage)
library(moments)
library(tidymodels)
library(nflfastR)

pbp <- purrr::map_df(c(2011:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

# This code chunk creates a dataframe that stores win, loss, tie, point differential info for all regular season games
# Create outcomes dataframe

regular_season_results <- function(pbp) {
  outcomes <- pbp %>%
    dplyr::filter(.data$season_type == "REG") %>%
    dplyr::group_by(.data$season, .data$game_id, .data$home_team) %>%
    dplyr::summarise(
      home_win = if_else(sum(.data$result) > 0, 1, 0),
      home_tie = if_else(sum(.data$result) == 0, 1, 0),
      home_diff = last(.data$result),
      home_pts_for = last(.data$home_score),
      home_pts_against = last(.data$away_score)
    ) %>%
    dplyr::group_by(.data$season, .data$home_team) %>%
    dplyr::summarise(
      home_games = n(),
      home_wins = sum(.data$home_win),
      home_ties = sum(.data$home_tie),
      home_diff = sum(.data$home_diff),
      home_pts_for = sum(.data$home_pts_for),
      home_pts_against = sum(.data$home_pts_against),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      # away games
      pbp %>%
        dplyr::filter(.data$season_type == "REG") %>%
        dplyr::group_by(.data$season, .data$game_id, .data$away_team) %>%
        dplyr::summarise(
          away_win = if_else(sum(.data$result) < 0, 1, 0),
          away_tie = if_else(sum(.data$result) == 0, 1, 0),
          away_diff = last(.data$result) * -1,
          away_pts_for = last(.data$away_score),
          away_pts_against = last(.data$home_score)
        ) %>%
        dplyr::group_by(.data$season, .data$away_team) %>%
        dplyr::summarise(
          away_games = n(),
          away_wins = sum(.data$away_win),
          away_ties = sum(.data$away_tie),
          away_diff = sum(.data$away_diff),
          away_pts_for = sum(.data$away_pts_for),
          away_pts_against = sum(.data$away_pts_against),
          .groups = "drop"
        ),
      by = c("season", "home_team" = "away_team")
    ) %>%
    dplyr::rename(team = "home_team") %>%
    dplyr::mutate(
      games = .data$home_games + .data$away_games,
      wins = .data$home_wins + .data$away_wins,
      losses = .data$games - .data$wins,
      ties = .data$home_ties + .data$away_ties,
      win_percentage = (.data$wins + 0.5 * .data$ties) / .data$games,
      point_diff = .data$home_diff + .data$away_diff,
      points_for = .data$home_pts_for + .data$away_pts_for,
      points_against = .data$home_pts_against + .data$away_pts_against,
      pythag_wins = (.data$points_for^2.37 / (.data$points_for^2.37 + .data$points_against^2.37)) * 16
    ) %>%
    dplyr::select(
      .data$season, .data$team, .data$games, .data$wins, .data$losses, .data$ties, .data$win_percentage,
      .data$point_diff, .data$points_for, .data$points_against, .data$pythag_wins
    )

  return(outcomes)
}

outcomes <- regular_season_results(pbp)

# This code chunk creates a dataframe that stores season long offensive and defensive stats

# Create metrics dataframe

regular_season_metrics <- function(pbp) {
  
  # <<- stores the values into your working space
  metrics <- pbp %>%
    dplyr::filter(
      .data$season_type == "REG",
      !is.na(.data$epa),
      .data$pass == 1 | .data$rush == 1
    ) %>%
    dplyr::group_by(.data$season, .data$posteam) %>%
    dplyr::summarise(
      n_pass = sum(.data$pass),
      n_rush = sum(.data$rush),
      pass_yards = sum(.data$yards_gained * .data$pass, na.rm = TRUE),
      rush_yards = sum(.data$yards_gained * .data$rush, na.rm = TRUE),
      epa_per_pass = sum(.data$epa * .data$pass) / .data$n_pass,
      epa_per_rush = sum(.data$epa * .data$rush) / .data$n_rush,
      success_per_pass = sum(.data$pass * .data$epa > 0) / .data$n_pass,
      success_per_rush = sum(.data$rush * .data$epa > 0) / .data$n_rush,
      y_per_pass = sum(.data$yards_gained * .data$pass, na.rm = TRUE) / .data$n_pass,
      y_per_rush = sum(.data$yards_gained * .data$rush, na.rm = TRUE) / .data$n_rush
    ) %>%
    dplyr::left_join(
      pbp %>%
        dplyr::filter(
          .data$season_type == "REG",
          !is.na(.data$epa),
          .data$pass == 1 | .data$rush == 1
        ) %>%
        dplyr::group_by(.data$season, .data$defteam) %>%
        dplyr::summarise(
          def_n_pass = sum(.data$pass),
          def_n_rush = sum(.data$rush),
          def_pass_yards = sum(.data$yards_gained * .data$pass, na.rm = TRUE),
          def_rush_yards = sum(.data$yards_gained * .data$rush, na.rm = TRUE),
          def_epa_per_pass = sum(-.data$epa * .data$pass) / .data$def_n_pass,
          def_epa_per_rush = sum(-.data$epa * .data$rush) / .data$def_n_rush,
          def_success_per_pass = sum(.data$pass * .data$epa > 0) / .data$def_n_pass,
          def_success_per_rush = sum(.data$rush * .data$epa > 0) / .data$def_n_rush,
          def_y_per_pass = sum(.data$yards_gained * .data$pass, na.rm = TRUE) / .data$def_n_pass,
          def_y_per_rush = sum(.data$yards_gained * .data$rush, na.rm = TRUE) / .data$def_n_rush
        ),
      by = c("season", "posteam" = "defteam")
    ) %>%
    dplyr::rename(team = "posteam") %>%
    dplyr::select(-c(.data$n_pass, .data$n_rush, .data$def_n_pass, .data$def_n_rush))
  
  return(metrics)

}

regular_season_metrics(pbp)

# Create dataframe for season long outcomes and stats

df <- outcomes %>% 
  left_join(metrics, by = c("season", "team"))

# Create simple linear regression based on all variables of interest and store r squared
# value of each fit in dataframe called r_squareds

models <- df %>% 
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = c(season:games, losses:def_y_per_rush),
    names_to = "measure_name",
    values_to = "measure_value"
  ) %>%
  split(.$measure_name) %>%
  map(~lm(wins ~ measure_value, data = .)) %>%
  tibble(
    metric = names(.),
    untidied = .
  ) %>%
  mutate(
    tidy = map(untidied, broom::tidy),
    glanced = map(untidied, glance),
    augmented = map(untidied, augment)
  ) 

models_glance <- models %>% 
  unnest(glanced)

models_glance %>%
  mutate(
    metric = str_replace_all(metric, "[\\s_]+", " ") %>%
      str_to_title() %>%
      str_replace_all(., "Epa", "EPA")
  ) %>%
  ggplot(aes(
    x = r.squared,
    y = reorder(metric, r.squared)
  )) +
  geom_col(fill = "royal blue") +
  labs(
    title = "R-Squared of Simple Linear Regressions",
    subtitle = "Wins Regressed on Individual Metrics | 2011 to 2021 NFL Seasons",
    x = element_blank(),
    y = "R-Squared",
    caption = "Data: nflfastR"
  ) +
  theme(
    plot.title = element_text(
      size = 16, hjust = 0.5,
      face = "bold", color = "black"),
    plot.subtitle = element_text(
      size = 10, hjust = 0.5,
      color = "black"),
    axis.title = element_text(
      size = 12, color = "black"),
    axis.text = element_text(
      size = 10, color = "black")
  )

#We can see passing efficiency metrics have the strongest relationships with wins. Furthermore, offensive passing efficiency metrics have 
#stronger relationships than defensive passing metrics do. 

#A team's expected points added per dropback explains nearly half of the variation in their season win total. Whereas defensive expected 
#points added per dropback explains about 32% of the variation in wins. Offensive and defensive rushing efficiency metrics only explain 
#about 18 and 9% of the variation in wins respectively. 

# Build basic random forest model


#We can also build a random forest model and let the model tell us what features yield the most information gain. Again, passing 
#efficiency is the largest driver of wins and it is not particularly close. 

set.seed(2017)
wins_split <- initial_split(select(df, wins, pass_yards:def_y_per_rush))
wins_train <- training(wins_split)
wins_test <- testing(wins_split)

rf_model <- randomForest::randomForest(wins ~ ., data = wins_train, ntree = 500, mtry = 16, importance = TRUE, 
                                       na.action = randomForest::na.roughfix, replace = FALSE)

rf_values <- rf_model[['importance']] %>%
  as_tibble() %>%
  mutate(
    metric = pull(models, metric) %>%
      str_replace_all(., "[\\s_]+", " ") %>%
      str_to_title() %>%
      str_replace_all(., "Epa", "EPA") %>%
      str_replace_all(., "Y ", "Yards ")
  ) %>%
  select(-`%IncMSE`)

# Create plot of feature importance from the random forest model
rf_values %>% 
  ggplot(aes(
    x = IncNodePurity,
    y = reorder(metric, IncNodePurity))) +
  geom_col() +
  labs(
    title = "NFL Season Wins Variable Importance",
    subtitle = "Random Forest Model | 2011 to 2021 NFL Seasons",
    x = element_blank(),
    y = "Variable Importance Score",
    caption = "Data: nflfastR"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 10, color = "black")
  )


aa_critique_fit <- function(fit) {
  
  # The object res stores our results as a list. Initialize it
  
  res <- list()
  
  # Use coefficients and intercept of the fit to create the fitted
  # regression equation/formula,
  
  coeffs <- coefficients(fit)
  res$formula <- paste0(
    as.character(formula(fit)[2]), " = ",
    round(coeffs[1], 4), " + ",
    paste(sprintf("%.4f * %s", coeffs[-1], names(coeffs[-1])), collapse=" + ")
  )
  
  # Store R's standard summary of the fit
  
  res$summary <- summary(fit)
  
  # Pull out the adjusted R^2, standard error, and mean Y values
  
  res$R2 <- res$summary$adj.r.squared
  res$Se <- res$summary$sigma
  res$mean_Y <- mean(fit$model[[1]], na.rm = TRUE)
  
  # Get the 95% confidence intervals for the coefficients and intercept
  
  res$confint <- confint(fit)
  
  # Construct the plot of residuals on fitted values
  
  res$residual_plot <- qplot(fit$fitted.values, fit$residuals) +
    xlab("Fitted Value") + ylab("Residual")
  
  # Construct the residual histogram (actually, residual density)
  
  res$residual_histogram <- qplot(fit$residuals, geom = "density") + xlab("Residual")
  
  # Store the correlation matrix using the above aa_cor() function
  
  res$cor <- aa_cor(fit$model)
  
  # If this is a multiple linear regression, calculate the
  # "generalized" VIFs
  
  # # if(length(fit$coefficients) > 2) {
  # if(length(labels(terms(fit))) > 1) {
  #     if(class(vif(fit)) == "numeric") res$vif <- vif(fit)
  #     else res$vif <- vif(fit)[, 1]
  # }
  
  # Return the results
  
  res
}



# Use all epa metrics to predict wins
fit <- lm(data = df, wins ~ epa_per_pass + epa_per_rush + def_epa_per_pass + def_epa_per_rush)
crit <- aa_critique_fit(fit)
#crit$summary  
#crit$residual_plot
#crit$residual_histogram

df <- df %>%
  mutate(
    pred = predict(fit, type = "response"),
    var = wins - pred,
    pythag_var = wins - pythag_wins
  )

df %>% 
  ggplot(aes(x = var)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "steelblue",
                args = list(
                  mean = pull(df, var) %>% mean,
                  sd = pull(df, var) %>% sd
                )) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "darkgrey",
                xlim = c(2.86, 4),
                args = list(
                  mean = pull(df, var) %>% mean,
                  sd = pull(df, var) %>% sd
                )) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "darkgrey",
                xlim = c(-2.86, -4),
                args = list(
                  mean = pull(df, var) %>% mean,
                  sd = pull(df, var) %>% sd
                )) +
  labs(
    title = "Distribution of Actual minus Expected Wins",
    subtitle = "Expected wins based on season EPA metrics | 1999 to 2019 Seasons",
    x = element_blank(),
    y = element_blank(),
    caption = "Data from @nflscrapR & @nflfastR"
  ) +
  theme(
    plot.title = element_text(size = 20,
                              hjust = 0.5,
                              face = "bold",
                              color = "black"),
    plot.subtitle = element_text(size = 14,
                                 hjust = 0.5,
                                 color = "black"),
    axis.text = element_text(size = 14,
                             color = "black"))

#We know offensive and defensive EPA per dropback metrics are useful for explaining season win totals. Just for fun make a linear regression model 
#that uses EPA per dropback and per rush for both sides of the ball. This regression explains 78% of the variation in season wins. 

#We can use the regression formula to develop expected wins based on EPA per play metrics. The distribution of actual wins minus expected wins is 
#normally distributed with a mean of 0 and a standard deviation of 1.4 wins. 

#This means 68% of the season win totals from 2009-2019 are plus or minus 1.4 wins from what our expected wins formula predicts. Furthermore, 95% 
#of the season win totals are within 3 games of what we would predict. Put another way, it is rare for a team to out or underperform their 
#expected wins by more than 3 games. 

# The variance of actual minus expected wins seems to be normally distributed
# What is the kurtosis?

#  kurtosis(df$var)

# Kurtosis of 2.9, normal distribution would be 3.
# So this distribution has a little less mass in the tails as normal distribution


## How did expected and actual wins look in 2019? 
df <- df %>% 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# Create function for plotting actual and expected wins for a season
plot_function <- function(df, szn) {
  df <- df %>% 
    filter(season == szn) %>% 
    arrange(-var)
  
  df$team <- factor(df$team, levels = df$team)
  
  df %>% 
    ggplot(aes(x = reorder(team, var), y = var)) +
    geom_bar(stat = "identity", aes(color = team, fill = team), show.legend = FALSE) +
    geom_image(aes(image = team_logo_espn), size = 0.03, asp = 16/9) +
    scale_color_manual(values = df$team_color2) +
    scale_fill_manual(values = df$team_color) +
    labs(caption = "Data from @nflscrapR & @nflfastR",
         title = paste(szn, "Actual Wins over Expected Wins"),
         subtitle = "Expected wins based on season EPA metrics",
         x = element_blank(),
         y = element_blank()) +
    theme(axis.title = element_text(size = 14,
                                    color = "black"),
          plot.subtitle = element_text(size = 12, 
                                       hjust = .5,
                                       color = "black"),
          axis.text = element_text(size = 14,
                                   color = "black"),
          plot.title = element_text(size = 14, 
                                    hjust = 0.5,
                                    face = "bold",
                                    color = "black"),
          plot.caption = element_text(size = 8,
                                      color = "black"),
          panel.background = element_rect(fill = "white",
                                          color = "black",
                                          size = 0.5),
          plot.background = element_rect(fill = "white")) +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    theme(axis.text.y = element_blank()) + 
    scale_y_continuous(breaks = c(-4:4))
}

plot_function(df, 2021)

extreme_teams <- function(df) {
  
  df <- df %>%
    arrange(team, season) %>% 
    mutate(
      lead_wins = ifelse(season == 2021, NA, lead(wins))
    ) %>% 
    filter(season < 2021) %>%
    mutate(
      lead_wins = as.double(lead_wins),
      pred_pythag_var = pred - pythag_wins
    ) %>%
    filter(var <= -2.5 | var >= 2.5) %>%
    mutate(
      range = if_else(var <= -2.5, "Bottom", "Top")
    ) %>%
    group_by(range) %>%
    summarise(
      wins = mean(wins),
      lead_wins = mean(lead_wins),
      variance = lead_wins - wins,
      n = n()
    )
  
  return(df)
  
}

extreme_teams(df)

 #Looking at the 25 teams in the right tail (those who over performed by more than 2.5 wins) from 1999 to 2018 we find that on average 
 #their wins dropped by 2.3 games in the next season. Not great news for Packers or Seahawks fans in 2020. 
 
 #The 29 teams n the left tail we see that teams who under performed by more than 2.5 wins increased their wins by 2.7 games the next 
 #season. The 2019 Cowboys, Chargers, and Buccaneers also fall into this tail.
 
#The difference between actual and expected wins is largely a function of how a team performs in one score games and on special teams performance. Record in one score games isn't very stable year over year for the most part, however, a few teams did consistently out or over perform their expected wins. 

#Of the 669 season long performances in the data only 38 teams under performed by more than 2.35 wins. The Chargers account for over a fifth of those seasons. 

 # Function to plot actual minus expected wins for a team over the years
 plot_function2 <- function(df, tm) {
  df <- df %>%
    filter(team == tm) %>%
    arrange(-var) %>%
    ggplot(aes(
      x = reorder(season, var),
      y = var
    )) +
    geom_bar(stat = "identity", aes(color = team, fill = team), show.legend = FALSE) +
    geom_image(aes(image = team_logo_espn), size = 0.03, asp = 16 / 9) +
    scale_color_manual(values = df$team_color2) +
    scale_fill_manual(values = df$team_color) +
    labs(
      caption = "Data: nflfastR",
      title = paste(tm, "Actual Wins over Expected Wins"),
      subtitle = "Expected wins based on season EPA metrics",
      x = element_blank(),
      y = element_blank()
    ) +
    theme(
      axis.title = element_text(
        size = 16,
        color = "black"
      ),
      plot.subtitle = element_text(
        size = 14,
        hjust = .5,
        color = "black"
      ),
      axis.text = element_text(
        size = 14,
        color = "black"
      ),
      plot.title = element_text(
        size = 24,
        hjust = 0.5,
        face = "bold",
        color = "black"
      ),
      plot.caption = element_text(
        size = 12,
        color = "black"
      ),
      panel.background = element_rect(
        fill = "white",
        color = "black",
        size = 0.5
      ),
      plot.background = element_rect(fill = "white")
    ) +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    theme(axis.text.y = element_blank()) +
    scale_y_continuous(breaks = c(-4:4)) +
    geom_text(aes(label = season, color = team_color2, fontface = "bold", size = 14),
      position = position_stack(.5), show.legend = FALSE
    )
}
 
 plot_function2(df, "LAC")