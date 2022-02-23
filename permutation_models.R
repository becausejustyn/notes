library(dplyr)
library(purrr)
library(broom)

mtcars_vars <- mtcars %>% names()

mtcars_formulas <- expand.grid(mtcars_vars, mtcars_vars, mtcars_vars) %>%
  filter(Var1 != Var2, Var2 != Var3, Var1 != Var3)

f <- function(Var1, Var2, Var3){
  paste(Var1, paste(Var2, Var3, sep = " + "), sep = " ~ ")
} 

formulas <- mtcars_formulas %>% 
  pmap_chr(f)

#iterative over
models <- map(formulas, ~ lm(., data = mtcars)) %>% 
  set_names(formulas) %>% 
  map_dfr(tidy, .id = ".x")