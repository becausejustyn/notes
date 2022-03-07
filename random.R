


money_line <- function(fav, underdog) {
  
  require(dplyr)
  
  fav_odds = (fav * -1)
  prob1 = fav_odds / (fav_odds + underdog)
  prob2 = underdog / (fav_odds + underdog)
  
  fav_prob = prob1 %>% round(3)
  underdog_prob = prob2 %>% round(3)
  
  return(glue::glue("Favourite odds: {fav_prob}\n Underdog odds:{underdog_prob}"))
}

# E.g. money_line(-130, 110)

money_line2 <- function(arg1, arg2) {
  
  fav = sort(c(arg1, arg2))[1]
  underdog = sort(c(arg1, arg2))[2]
  
  fav_val = fav * -1
  
  fav_prob = fav_val / (fav_val + underdog)
  
  return(fav_prob %>% round(3))
}

# money_line2(-425, 351)




logit <- function(p) { 
  out <- log(p/(1 - p))
  return(out)
}



schedule <- nflreadr::load_schedules(seasons = 2021)

schedule <- schedule %>%
  #filter(game_type == 'REG') %>%
  select(season, week, gameday, weekday, gametime, 
         home_team, home_score, away_team, away_score,
         home_moneyline, away_moneyline,
         spread_line, 
         total_line,
         result, total, 
         home_rest, away_rest,
         div_game, roof, surface
         ) %>%
  rowwise() %>%
  mutate(
    home_win_prob = money_line2(home_moneyline, away_moneyline),
    away_win_prob = 1 - (money_line2(home_moneyline, away_moneyline)),
    total_hit = if_else(total > total_line, 1, 0),
    home_covered = if_else(
      # home fav                               home underdog
      spread_line > 0 & result > spread_line | spread_line < 0 & result > spread, 1, 0),
      
    away_covered = if_else(
      #away fav                                #away underdog
      spread_line < 0 & result < spread_line | spread_line > 0 & (result * -1) > spread_line
      
    )
    away_covered = case_when(
      #away fav
      spread_line < 0 & result < spread_line ~ 1,
      #away underdog
      spread_line > 0 & (result * -1) > spread_line ~ 1,
      TRUE ~ 0)
  )


home_covered = if_else(spread_line > 0 & (result*-1) > spread_line, 1, 0),

home_covered = if_else(spread_line > 0 & (result*-1) > spread_line, 1, 0),

away_covered = if_else(spread_line < 0 & (result*-1) < spread_line, 1, 0)
#away_covered = if_else(spread_line < 0 & (result*-1) > spread_line, 1, 0)





schedule <- schedule %>%
  #filter(game_type == 'REG') %>%
  select(season, week, gameday, weekday, gametime, 
         home_team, home_score, away_team, away_score,
         home_moneyline, away_moneyline,
         spread_line, 
         total_line,
         result, total, 
         home_rest, away_rest,
         div_game, roof, surface
  ) %>%
  rowwise() %>%
  mutate(
    home_win_prob = money_line2(home_moneyline, away_moneyline),
    away_win_prob = 1 - (money_line2(home_moneyline, away_moneyline)),
    total_hit = if_else(total > total_line, 1, 0),
    
    home_covered = case_when(
      #home fav
      spread_line > 0 & result > spread_line ~ 1,
      #home underdog
      spread_line < 0 & result > spread_line ~ 1,
      TRUE ~ 0
    ),
    
    away_covered = case_when(
      #away fav
      spread_line < 0 & result < spread_line ~ 1,
      #away underdog
      spread_line > 0 & (result * -1) > spread_line ~ 1,
      TRUE ~ 0)
  )


spread_line1 <- 10
result1 <- 2

spread_line1 > 0 & result1 > spread_line1
spread_line1 < 0 & result1 > spread_line1
spread_line1 < 0 & result1 < spread_line1

spread_line1 > 0 & (result1 * -1) > spread_line1



#using the spread as your prob, then using bayes

library(gsheet)
library(lubridate)

urlA <- "https://docs.google.com/spreadsheets/d/1Hsmqphn9kGqWa88aYnimQxOvdV7ulv_jeIj0SYeBr3s/edit?usp=sharing"
nhl1718 <- gsheet2tbl(urlA)


## Improve the date format
nhl1718 <- nhl1718 %>% 
  separate(date, c("day", "month"), "-") %>% 
  mutate(
    date = ymd(paste(year, month, day))
    )

min.day <- pull(nhl1718, date) %>% min()

nhl1718 <- nhl1718 %>%
  mutate(
    day = date - min.day, week = as.numeric(floor(day / 7) + 1)
    )

y <- logit(nhl1718[['p_home']])
w <- nhl1718[['week']]


#create a design matrix 
Teams <- nhl1718[['home']] %>% unique() %>% sort()


#Defining the number of things
nTeams <- length(Teams)
nWeeks <- max(nhl1718[['week']])
n <- nrow(nhl1718)

#Defining the design matrix
x <- matrix(0, nrow = dim(nhl1718)[1], ncol = length(Teams))

for (i in 1:dim(nhl1718)[1]) {
  x[i, which(as.character(nhl1718[i,"home"]) == Teams)] <- (1)
  x[i, which(as.character(nhl1718[i,"away"]) == Teams)] <- (-1)
} 


library(rjags)


model.string <-"
  model { 
for (i in 1:n) {
  y[i] ~ dnorm(mu[i], tauGame)
  mu[i] <- alpha + inprod(theta[w[i],],x[i,])
}
for (j in 1:nTeams){
  theta[1,j] ~ dnorm(0, tauSeason)
}
for (www in 2:nWeeks) {  
  for (j in 1:nTeams) {
    theta[www,j] ~ dnorm(gammaWeek*theta[www-1,j], tauWeek)
  }
}
alpha ~ dnorm(0,0.0001)
tauGame ~ dunif(0,1000) #uncertainty in outcome for each game
tauWeek ~ dunif(0,1000) 
tauSeason ~ dunif(0,1000) #variance parameter for the first week of the season
gammaWeek ~ dunif(0,1.5)
}
"
model.spec<-textConnection(model.string)

n.chains <- 3 
n.adapt <- n.update <- n.draws <- 1000

posteriorDraws = c('alpha','theta')
thin <- 5

jags <- jags.model(model.spec,
  data = list("y" = y, "x" = x, "w" = w, "n" = n, "nTeams" = nTeams, "nWeeks" = nWeeks),
  n.chains = n.chains, n.adapt = n.adapt
)


update(jags, n.update)
z <- jags.samples(jags, posteriorDraws, n.draws, thin = thin)

z[['alpha']] %>% dim()
z[['theta']] %>% dim()

#It’s generally a good idea to start a Bayesian analysis by assessing convergence

colours <- c("#7fc97f", "#beaed4", "#fdc086")
hfas <- data.frame(round(z$alpha[,,], 3))  %>% mutate(draw = 1:n())
hfas %>% ggplot(aes(draw, X1)) +
  geom_line(colour = colours[1]) + 
  geom_line(data = hfas, aes(draw, X2), colour = colours[2]) + 
  geom_line(data = hfas, aes(draw, X3), colour = colours[3]) + 
  xlab("Draw") + 
  ggtitle("Home advantage (logit scale)") + 
  ylab("") + 
  theme_bw()

#The posterior distrbution of alpha is centered at about 0.208. This suggests that home teams, when playing a 
# similarly talented opponent, have about a \frac{e^{0.208}}{1 + e^{0.208}} = 55% chance of winning.