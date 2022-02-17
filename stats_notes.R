
library(dplyr)
library(infer)

# sim

n_simu <- 2
n_obs <- 3

data.frame(obs_id = 1:n_obs) %>%
  dplyr::slice(rep(obs_id, each = n_simu)) %>% # each n_simu times
  mutate(simu_id = rep(1:n_simu, n_obs)) %>% # create simu_id
  arrange(simu_id, obs_id) %>% # obs_id are nested within simu_id
  select(simu_id, obs_id)


# hi

data.frame(obs_id = 1:n_obs) %>%
  dplyr::slice(rep(obs_id, each = n_simu)) %>%
  mutate(
    simu_id  = rep(1:n_simu, n_obs),
    pois_var = rpois(dplyr::n(), 10) # simulated online_views
  ) %>%
  arrange(simu_id, obs_id) %>%
  select(simu_id, obs_id, pois_var) %>%
  rename(car_id = obs_id, online_views = pois_var)

# Replicate

replicate(10, mean(rnorm(100)))

# The tidy way with map

tibble(rep = 1:10) %>%
  mutate(samples = map(rep, ~ rnorm(100))) %>%
  mutate(means = map_dbl(samples, ~ mean(.)))

# purrr

rerun(10, rnorm(100)) %>%
  map_dbl(~ mean(.))


# Power of t-test

rerun(1000, rnorm(15, 8, 4)) %>%
  map_dbl(~ t.test(., mu = 10)$p.value) -> pvals
table(pvals <= 0.05)


power.t.test(n = 15, delta = 8 - 10, sd = 4, type = "one.sample", alternative = "two.sided")
