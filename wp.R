
library(tidyverse)
library(hexbin)


library(magrittr)
library(ggthemes)
library(wesanderson)


game_results <- pbp %>%
  group_by(game_id) %>%
  filter(str_detect(desc, "END GAME") == TRUE) %>%
  select(game_id, home_team, away_team, home_score, away_score)

new_pbp <- pbp %>%
  select(game_id, home_team, away_team, posteam, wp, posteam_score, game_seconds_remaining) %>%
  left_join(game_results, .) %>%
  mutate(
    winning_team = if_else(home_team > away_team, 1, 0),
    wp_held = if_else(posteam == winning_team, 1, 0),
    wp_err = if_else(posteam == winning_team, 1 - wp, wp)
  ) %>%
  filter(!is.na(wp))

# https://twitter.com/GrumpyNFLStats/status/1310703614495567872?s=20

new_pbp %>% 
  filter(!is.na(wp_held)) %>%
  ggplot(aes(
    x = game_seconds_remaining, 
    y = wp, 
    z = wp_held)
    ) + 
  stat_summary_hex(
    bins = 120, color = "grey", fun = mean
    ) + 
  scale_x_reverse() + 
  theme_bw() +
  labs(
    title = "Observed probability that result indicated by WP model holds till end of regulation",
    subtitle = "Teams with a model-implied lead are, in fact, quite likely to win,\n and vice-versa, no matter how much time is left on the clock",
    caption = "data: nflfastR"
  )

# https://twitter.com/GrumpyNFLStats/status/1310703618660597760?s=20

new_pbp %>% 
  ggplot(aes(
    x = game_seconds_remaining, 
    y = wp, 
    z = wp_err)) + 
  stat_summary_hex(bins = 120, 
                   color = "grey", 
                   fun = mean) + 
  scale_x_reverse() + 
  theme_bw() +
  labs(
    title = "Observed probability that result indicated by WP model does not hold till end-of-regulation",
    subtitle = "If the estimated win probability is between 0.33 and 0.66, the game could very well turn",
    caption = "data: nflfastR"
  )

# https://twitter.com/GrumpyNFLStats/status/1310703622968098816?s=20

new_pbp %>% 
  ggplot(aes(x = game_seconds_remaining, y = wp)) + 
  geom_hex(aes(fill = stat(sqrt(count))), bins = 120) +
  viridis::scale_fill_viridis() + scale_x_reverse() + 
  theme_bw() +
  labs(title = "Density of WP estimates by time remaining in the game",
       subtitle = "WP estimates are quite evenly distributed - no bias evident",
       caption = "CC-BY-SA @GrumpyNFLStats - data by @nflFastR"
  )

# bonus plot - not tweeted
# model error is ~sorta well behaved - some weirdness evident but bears more analysis

new_pbp %>% 
  ggplot(aes(x = game_seconds_remaining, y = wp, z = wp_err)) + 
  stat_summary_hex(bins = 120, 
                   color = "grey", 
                   fun = sd) + 
  scale_x_reverse() + 
  theme_bw() +
  labs(title = "Observed probability that result indicated by WP model does not hold till end-of-regulation",
       subtitle = "If the estimated win probability is between 0.33 and 0.66, the game could very well turn",
       caption = "CC-BY-SA @GrumpyNFLStats - data by @nflFastR"
  )