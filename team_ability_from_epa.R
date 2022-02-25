

# Estimating Team Ability From EPA

# https://github.com/nflverse/open-source-football/blob/master/_posts/2021-06-27-estimating-team-ability-from-epa/estimating-team-ability-from-epa.Rmd
# https://www.opensourcefootball.com/posts/2021-06-27-estimating-team-ability-from-epa/

library(tidyverse)

pbp <- purrr::map_df(c(2018:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

library(rstan)
library(lme4)
library(tidyverse)
library(DT)
library(tidybayes)
library(ggrepel)
library(magick)
library(resample)


post16 <- filter(
  pbp,
  season_type == "REG" &
    !is.na(epa) &
    play_type %in% c("pass", "run") &
    down %in% c(1, 2, 3, 4)
) %>%
  dplyr::select(season, week, posteam, defteam, epa, play_id, qtr, quarter_seconds_remaining) %>%
  mutate(
    def_id = as.numeric(factor(str_c(defteam, "-", season))),
    off_id = as.numeric(factor(str_c(posteam, "-", season)))
  )

epa_off_actual <- post16 %>%
  group_by(posteam, season, off_id) %>%
  summarise(
    offensive_epa = mean(epa),
    n_offense = n()
  )

epa_def_actual <- post16 %>%
  group_by(defteam, season, def_id) %>%
  summarise(defensive_epa = mean(epa))

epa_actual <- epa_off_actual %>%
  inner_join(epa_def_actual, by = c("posteam" = "defteam", "season", "off_id" = "def_id"))

chiefs_2019 <- filter(post16, week <= 4 & season == 2019 & (posteam == "KC" | defteam == "KC")) %>%
  arrange(week, play_id) %>%
  mutate(
    play_num = row_number(),
    bp = ifelse(epa < -13, 1, 0)
  )

big_play_id <- which(chiefs_2019$bp == 1)

pre <- chiefs_2019 %>%
  filter(
    play_num < big_play_id,
    defteam == "KC"
  ) %>%
  summarise(epa = mean(epa))

with <- chiefs_2019 %>%
  filter(
    play_num <= big_play_id,
    defteam == "KC"
  ) %>%
  summarise(epa = mean(epa))


stanmod <- stan_model("epa_per_play_off_only.stan")

standat <- list(
  N = nrow(post16),
  I = length(unique(post16$off_id)),
  ii = post16$off_id,
  y = post16$epa
)

fit_off <- sampling(stanmod, data = standat, cores = 4, chains = 4, iter = 2000)



alpha_off <- rstan::extract(fit_off, pars = c("alpha_off"))$alpha_off
offense <- colMeans(alpha_off)
offense_se <- sqrt(colVars(alpha_off))
epa_actual$offense_only_estimate <- offense
epa_actual$offense_only_se <- offense_se
epa_actual$offense_only_lower <- epa_actual$offense_only_estimate - epa_actual$offense_only_se
epa_actual$offense_only_upper <- epa_actual$offense_only_estimate + epa_actual$offense_only_se

filter(epa_actual, season == 2020) %>%
  ggplot(aes(y = reorder(posteam, offense_only_estimate), x = offensive_epa)) +
  geom_point(shape = 1) +
  geom_point(aes(x = offense_only_estimate, y = posteam), colour = "blue") +
  geom_linerange(aes(xmin = offense_only_lower, xmax = offense_only_upper)) +
  theme_minimal() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    title = "Estimated vs. Actual Offensive EPA/Play, 2020",
    subtitle = "Model Estimate in Blue, +/- 1 s.e."
  ) +
  ylab("Team") +
  xlab("Offensive EPA")


# Adding Defense


# epa_per_play_normal.stan

stanmod_normal <- stan_model("epa_per_play_normal.stan")

standat_normal <- list(
  N = nrow(post16),
  I = length(unique(post16$off_id)),
  ii = post16$off_id,
  jj = post16$def_id,
  y = post16$epa,
  N_rep = nrow(filter(post16, season == 2020)),
  ii_rep = filter(post16, season == 2020)$off_id,
  jj_rep = filter(post16, season == 2020)$def_id
)

fit_normal <- sampling(stanmod_normal, data = standat_normal, cores = 4, chains = 4, iter = 2000)



offense_normal <- apply(rstan::extract(fit_normal, pars = c("alpha_off"))$alpha_off, 2, mean)
defense_normal <- apply(rstan::extract(fit_normal, pars = c("alpha_def"))$alpha_def, 2, mean)
epa_actual$offense_normal_estimate <- offense_normal
epa_actual$defense_normal_estimate <- defense_normal
alpha_off_normal <- rstan::extract(fit_normal, pars = c("alpha_off"))$alpha_off
alpha_def_normal <- rstan::extract(fit_normal, pars = c("alpha_def"))$alpha_def
offense_normal <- colMeans(alpha_off_normal)
offense_se_normal <- sqrt(colVars(alpha_off_normal))
defense_normal <- colMeans(alpha_def_normal)
defense_se_normal <- sqrt(colVars(alpha_def_normal))
epa_actual$offense_normal_estimate <- offense_normal
epa_actual$offense_normal_se <- offense_se_normal
epa_actual$offense_normal_lower <- epa_actual$offense_normal_estimate - epa_actual$offense_normal_se
epa_actual$offense_normal_upper <- epa_actual$offense_normal_estimate + epa_actual$offense_normal_se
epa_actual$defense_normal_estimate <- defense_normal
epa_actual$defense_normal_se <- defense_se_normal
epa_actual$defense_normal_lower <- epa_actual$defense_normal_estimate - epa_actual$defense_normal_se
epa_actual$defense_normal_upper <- epa_actual$defense_normal_estimate + epa_actual$defense_normal_se
print(fit_normal, pars = c("sigma_off", "sigma_def", "sigma_y"))


filter(epa_actual, season == 2021) %>%
  ggplot(aes(y = reorder(posteam, offense_normal_estimate), x = offensive_epa)) +
  geom_point(shape = 1) +
  geom_point(aes(x = offense_normal_estimate, y = posteam), colour = "blue") +
  geom_linerange(aes(xmin = offense_normal_lower, xmax = offense_normal_upper)) +
  theme_minimal() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    title = "Estimated vs. Actual Offensive EPA/Play, 2021",
    subtitle = "Model Estimate in Blue, +/- 1 s.e."
  ) +
  ylab("Team") +
  xlab("Offensive EPA/Play")

filter(epa_actual, season == 2021) %>%
  ggplot(aes(y = reorder(posteam, desc(defense_normal_estimate)), x = defensive_epa)) +
  geom_point(shape = 1) +
  geom_point(aes(x = defense_normal_estimate, y = posteam), colour = "blue") +
  geom_linerange(aes(xmin = defense_normal_lower, xmax = defense_normal_upper)) +
  theme_minimal() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    title = "Estimated vs. Actual Defensive EPA/Play, 2021",
    subtitle = "Model Estimate in Blue, +/- 1 s.e."
  ) +
  ylab("Team") +
  xlab("Defensive EPA/Play")



epa_actual %>%
  dplyr::left_join(
    nflfastR::teams_colors_logos %>% dplyr::select(team_abbr, team_logo_espn),
    by = c("posteam" = "team_abbr")
  ) %>%
  dplyr::mutate(
    grob = purrr::map(seq_along(team_logo_espn), function(x) {
      grid::rasterGrob(magick::image_read(team_logo_espn[[x]]))
    })
  ) %>%
  filter(season == 2021) %>%
  ggplot(aes(x = offense_only_estimate, y = offense_normal_estimate)) +
  ggpmisc::geom_grob(aes(x = offense_only_estimate, y = offense_normal_estimate, label = grob), vp.width = 0.05) +
  theme_minimal() +
  geom_smooth(method = "lm", se = F) +
  xlab("Offense Only Model") +
  ylab("Offense + Defense Model") +
  labs(
    title = "Estimated Offensive EPA/Play",
    subtitle = ""
  )


yrep <- rstan::extract(fit_normal, pars = "y_rep")$y_rep[1:100, ]

bayesplot::ppc_dens_overlay(post16$epa[post16$season == 2020], yrep) +
  labs(title = "Posterior Predictive Check") +
  xlim(-16, 10)

# Using the Student’s T

stanmod_t <- stan_model("epa_per_play.stan")

standat_t <- list(
  N = nrow(post16),
  I = length(unique(post16$off_id)),
  ii = post16$off_id,
  jj = post16$def_id,
  df = 6,
  y = post16$epa,
  N_rep = nrow(filter(post16, season == 2020)),
  ii_rep = filter(post16, season == 2020)$off_id,
  jj_rep = filter(post16, season == 2020)$def_id
)

fit_t <- sampling(stanmod_t, data = standat_t, cores = 4, chains = 4, iter = 2000)


epa_actual %>%
  dplyr::left_join(
    nflfastR::teams_colors_logos %>% dplyr::select(team_abbr, team_logo_espn),
    by = c("posteam" = "team_abbr")
  ) %>%
  dplyr::mutate(
    grob = purrr::map(seq_along(team_logo_espn), function(x) {
      grid::rasterGrob(magick::image_read(team_logo_espn[[x]]))
    })
  ) %>%
  filter(season == 2020) %>%
  ggplot(aes(x = offense_normal_estimate, y = offense_t_estimate)) +
  ggpmisc::geom_grob(aes(x = offense_normal_estimate, y = offense_t_estimate, label = grob), vp.width = 0.05) +
  theme_minimal() +
  geom_smooth(method = "lm", se = F) +
  xlab("Normal Model") +
  ylab("Student's t Model") +
  labs(
    title = "Estimated Offensive EPA/Play",
    subtitle = ""
  )


filter(post16, season == 2020 & defteam %in% c("MIA", "SF")) %>%
  ggplot(aes(x = epa, colour = defteam)) +
  scale_colour_manual(values = c("#008e97", "#aa0000")) +
  geom_density() +
  theme_minimal() +
  xlab("EPA") +
  ylab("") +
  labs(
    title = "Defensive EPA, Miami and San Francisco",
    colour = "Team"
  )



# Team Estimates

epa_actual %>%
  dplyr::select(posteam, season, offense_normal_estimate, offense_t_estimate, defense_normal_estimate, defense_t_estimate) %>%
  mutate_if(is.numeric, round, 2) %>%
  arrange(desc(offense_t_estimate)) %>%
  rename(
    "Team" = posteam,
    "Season" = season,
    "Offense, Normal" = offense_normal_estimate,
    "Offense, T" = offense_t_estimate,
    "Defense, Normal" = defense_normal_estimate,
    "Defense, T" = defense_t_estimate
  ) %>%
  datatable(filter = "top")