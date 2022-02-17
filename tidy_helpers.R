# Across functions


df %>%
  summarise(across(c(column1, column2, column3), 
                   n_distinct))

df %>%
  summarise(across(everything(), n_distinct))

df %>%
  summarise(across(contains("word"), n_distinct))

df %>%
  summarise(across(where(is.numeric), n_distinct))

df %>%
  summarise(across(everything(), 
                   ~sum(is.na(.))))

#replace NA with 0
replace0 <- function(x) {
  if_else(condition = is.na(x), 
          true = 0, 
          false = as.numeric(x))
}

df %>%
  mutate(across(where(is.numeric), replace0))

df %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = T), as.numeric(.))))

#replacing missing values with mean value of the group

df %>%
  group_by(group1, group2) %>%
  mutate(across(where(is.numeric), 
                ~if_else(condition = is.na(.), 
                         true = mean(., na.rm = T), 
                         false = as.numeric(.)))) %>%
  ungroup()

df %>%
  select(where(is.numeric))

# Interations

library(tidyverse)
library(corrr)

## Purrr

mtcars %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::group_by() %>%
  group_map(~ correlate(.x, quiet = TRUE))

mtcars %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::group_by() %>%
  group_map(~ cor(.x)) 

map_dbl(mtcars, mean, trim = 0.5)
