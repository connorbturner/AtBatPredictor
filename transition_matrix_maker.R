# Load necessary packages
library(baseballr)
library(tidyverse)
library(ggthemes)
library(expm)
library(data.table)

# Example hitters
trout <- 545361 # Mike Trout, OF, Los Angeles Angels
marte <- 606466 # Ketel Marte, 2B, Arizona Diamondbacks
alonso <- 624413 # Pete Alonso, 1B, New York Mets
buxton <- 621439 # Byron Buxton, OF, Minnesota Twins

# Example pitchers
verlander <- 434378 # Justin Verlander, P, Houston Astros
hendricks <- 543294 # Dartmouth Alum Kyle Hendricks, P, Chicago Cubs
castillo <- 622491 # Luis Castillo, P, Cincinnati Reds


# Batter Transition Matrix ------------------------------------------------

battermatrix <- function(playerid){
  
  ## Step 1: Aquire Player Data
  player_data <- scrape_statcast_savant_batter(start_date = "2019-03-28",
                                               end_date = "2019-09-29",
                                               batterid = playerid) %>%
    arrange(game_date, at_bat_number, pitch_number)
  
  ## Step 2: Add At-Bat Numbers
  i = 1
  player_data$at_bat = NA
  player_data$at_bat[1] = 1
  
  for (j in 2:nrow(player_data)) {
    if (player_data$at_bat_number[j] == player_data$at_bat_number[j-1]) {
      player_data$at_bat[j] = i
    }
    
    if (player_data$at_bat_number[j] != player_data$at_bat_number[j-1]) {
      i = i+1
      player_data$at_bat[j] = i
    }
    
  }
  
  ## Step 3: Select Relevant Variables and Tidy Data Set
  pitch_type_data <- player_data %>%
    select(game_date, home_team, at_bat, player_name, batter, pitcher, 
           stand, p_throws, pitch_number, pitch_type, pitch_name, balls, 
           strikes, events, description) %>%
    rename(current_pitch = pitch_name) %>%
    mutate(events = if_else(is.na(events), "none", events),
           previous_pitch = if_else(pitch_number == 1, "none", 
                                    lag(current_pitch)),
           count = paste0(as.character(balls), "-", as.character(strikes)),
           ball = if_else(grepl("ball", description), 1, 0),
           strike = if_else(grepl("strike", description), 1, 0),
           foul = if_else(grepl("foul", description), 1, 0),
           strike = if_else(foul == 1 & !grepl("-2", count), 1, strike),
           foul = if_else(description == "foul_tip" & 
                            events == "strikeout", 
                          0, foul),
           walk = if_else(events == "walk", 1, 0),
           strikeout = if_else(grepl("strikeout", events), 1, 0),
           oobip = if_else(grepl("field", events)|
                             grepl("grounded", events)|
                             grepl("force", events)|
                             grepl("sac", events), 
                           1, 0),
           single = if_else(events == "single", 1, 0),
           double = if_else(events == "double", 1, 0),
           triple = if_else(events == "triple", 1, 0),
           homerun = if_else(events == "home_run", 1, 0))
  
  ## Step 4: Filter Data for Relevant Outcomes
  filter_data <- pitch_type_data
  
  for (i in 1:nrow(pitch_type_data)) {
    ab = pitch_type_data$at_bat[i]
    if (pitch_type_data$events[i] == "hit_by_pitch") {
      filter_data <- filter_data %>%
        filter(at_bat != ab)
    }
  }
  
  ## Step 5: Set Outcome Probabilities for Each Count
  zerozero <- filter_data %>%
    filter(count == "0-0") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  zeroone <- filter_data %>%
    filter(count == "0-1") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  onezero <- filter_data %>%
    filter(count == "1-0") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  zerotwo <- filter_data %>%
    filter(count == "0-2") %>%
    summarize(n(), sum(strikeout), sum(ball), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           ball = 'sum(ball)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           ball = ball / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  oneone <- filter_data %>%
    filter(count == "1-1") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  twozero <- filter_data %>%
    filter(count == "2-0") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  onetwo <- filter_data %>%
    filter(count == "1-2") %>%
    summarize(n(), sum(strikeout), sum(ball), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           ball = 'sum(ball)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           ball = ball / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  twoone <- filter_data %>%
    filter(count == "2-1") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  threezero <- filter_data %>%
    filter(count == "3-0") %>%
    summarize(n(), sum(strike), sum(walk), sum(single), sum(double),
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           walk = 'sum(walk)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           walk = walk / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  twotwo <- filter_data %>%
    filter(count == "2-2") %>%
    summarize(n(), sum(strikeout), sum(ball), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           ball = 'sum(ball)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           ball = ball / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  threeone <- filter_data %>%
    filter(count == "3-1") %>%
    summarize(n(), sum(strike), sum(walk), sum(single), sum(double),
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           walk = 'sum(walk)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           walk = walk / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  threetwo <- filter_data %>%
    filter(count == "3-2") %>%
    summarize(n(), sum(strikeout), sum(walk), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           walk = 'sum(walk)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           walk = walk / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  ## Step 6: Build Transition Matrix
  outcome00 <- c(0, zerozero$strike[1], zerozero$ball[1], 0, 0, 0, 0, 0, 0,
                 0, 0, 0, zerozero$single[1], zerozero$double[1], 
                 zerozero$triple[1], zerozero$homerun[1], zerozero$oobip[1], 
                 0, 0)
  
  outcome01 <- c(0, 0, 0, zeroone$strike[1], zeroone$ball[1], 0, 0, 0, 0, 0,
                 0, 0, zeroone$single[1], zeroone$double[1], 
                 zeroone$triple[1], zeroone$homerun[1], zeroone$oobip[1], 
                 0, 0)
  
  outcome10 <- c(0, 0, 0, 0, onezero$strike[1], onezero$ball[1], 0, 0, 0, 0, 
                 0, 0, onezero$single[1], onezero$double[1], 
                 onezero$triple[1], onezero$homerun[1], onezero$oobip[1], 
                 0, 0)
  
  outcome02 <- c(0, 0, 0, zerotwo$foul[1], 0, 0, zerotwo$ball[1], 0, 0, 0, 
                 0, 0, zerotwo$single[1], zerotwo$double[1], 
                 zerotwo$triple[1], zerotwo$homerun[1], zerotwo$oobip[1], 
                 0, zerotwo$strikeout[1])
  
  outcome11 <- c(0, 0, 0, 0, 0, 0, oneone$strike[1], oneone$ball[1], 0, 0, 
                 0, 0, oneone$single[1], oneone$double[1], oneone$triple[1],
                 oneone$homerun[1], oneone$oobip[1], 0, 0)
  
  outcome20 <- c(0, 0, 0, 0, 0, 0, 0, twozero$strike[1], twozero$ball[1], 0, 
                 0, 0, twozero$single[1], twozero$double[1], 
                 twozero$triple[1], twozero$homerun[1], twozero$oobip[1], 
                 0, 0)
  
  outcome12 <- c(0, 0, 0, 0, 0, 0, onetwo$foul[1], 0, 0, onetwo$ball[1], 0, 
                 0, onetwo$single[1], onetwo$double[1], onetwo$triple[1],
                 onetwo$homerun[1], onetwo$oobip[1], 0, onetwo$strikeout[1])
  
  outcome21 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, twoone$strike[1], 
                 twoone$ball[1], 0, twoone$single[1], twoone$double[1], 
                 twoone$triple[1], twoone$homerun[1], twoone$oobip[1], 0, 0)
  
  outcome30 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, threezero$strike[1], 0,
                 threezero$single[1], threezero$double[1], 
                 threezero$triple[1], threezero$homerun[1], 
                 threezero$oobip[1], threezero$walk[1], 0)
  
  outcome22 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, twotwo$foul[1], 0, 
                 twotwo$ball[1], twotwo$single[1], twotwo$double[1], 
                 twotwo$triple[1], twotwo$homerun[1], twotwo$oobip[1], 
                 0, twotwo$strikeout[1])
  
  outcome31 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, threeone$strike[1],
                 threeone$single[1], threeone$double[1], threeone$triple[1],
                 threeone$homerun[1], threeone$oobip[1], 
                 threeone$walk[1], 0)
  
  outcome32 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, threetwo$foul[1], 
                 threetwo$single[1], threetwo$double[1], threetwo$triple[1],
                 threetwo$homerun[1], threetwo$oobip[1], threetwo$walk[1], 
                 threetwo$strikeout)
  
  outcome1B <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
  outcome2B <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
  outcome3B <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
  outcomeHR <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
  outcomeBP <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
  outcomeKK <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  outcomeBB <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
  transition <- matrix(rbind(outcome00, outcome01, outcome10, outcome02, 
                             outcome11, outcome20, outcome12, outcome21, 
                             outcome30, outcome22, outcome31, outcome32, 
                             outcome1B, outcome2B, outcome3B, outcomeHR, 
                             outcomeBP, outcomeKK, outcomeBB), 
                       nrow = 19,
                       ncol = 19)
  rownames(transition) <- c("0-0", "0-1", "1-0", "0-2", "1-1", "2-0", "1-2",
                            "2-1", "3-0", "2-2", "3-1", "3-2", "1B", "2B", 
                            "3B", "HR", "BIP", "BB", "K")
  colnames(transition) <- c("0-0", "0-1", "1-0", "0-2", "1-1", "2-0", "1-2",
                            "2-1", "3-0", "2-2", "3-1", "3-2", "1B", "2B", 
                            "3B", "HR", "BIP", "BB", "K")
  
  return(transition)
}


# Pitcher Transition Matrix ----------------------------------------------------------

pitchermatrix <- function(playerid, batterid){
  
  ## Step 1: Aquire Player Data
  player_data <- scrape_statcast_savant_pitcher(start_date = "2019-03-28",
                                                end_date = "2019-09-29",
                                                pitcherid = playerid) %>%
    arrange(game_date, at_bat_number, pitch_number) %>% 
    filter(batter != batterid)
  
  ## Step 2: Add At-Bat Numbers
  i = 1
  player_data$at_bat = NA
  player_data$at_bat[1] = 1
  
  for (j in 2:nrow(player_data)) {
    if (player_data$at_bat_number[j] == player_data$at_bat_number[j-1]) {
      player_data$at_bat[j] = i
    }
    
    if (player_data$at_bat_number[j] != player_data$at_bat_number[j-1]) {
      i = i+1
      player_data$at_bat[j] = i
    }
    
  }
  
  ## Step 3: Select Relevant Variables and Tidy Data Set
  pitch_type_data <- player_data %>%
    select(game_date, home_team, at_bat, player_name, batter, pitcher, 
           stand, p_throws, pitch_number, pitch_type, pitch_name, balls, 
           strikes, events, description) %>%
    rename(current_pitch = pitch_name) %>%
    mutate(events = if_else(is.na(events), "none", events),
           previous_pitch = if_else(pitch_number == 1, "none", 
                                    lag(current_pitch)),
           count = paste0(as.character(balls), "-", as.character(strikes)),
           ball = if_else(grepl("ball", description), 1, 0),
           strike = if_else(grepl("strike", description), 1, 0),
           foul = if_else(grepl("foul", description), 1, 0),
           strike = if_else(foul == 1 & !grepl("-2", count), 1, strike),
           foul = if_else(description == "foul_tip" & 
                            events == "strikeout", 
                          0, foul),
           walk = if_else(events == "walk", 1, 0),
           strikeout = if_else(grepl("strikeout", events), 1, 0),
           oobip = if_else(grepl("field", events)|
                             grepl("grounded", events)|
                             grepl("force", events)|
                             grepl("sac", events), 
                           1, 0),
           single = if_else(events == "single", 1, 0),
           double = if_else(events == "double", 1, 0),
           triple = if_else(events == "triple", 1, 0),
           homerun = if_else(events == "home_run", 1, 0))
  
  ## Step 4: Filter Data for Relevant Outcomes
  filter_data <- pitch_type_data
  
  for (i in 1:nrow(pitch_type_data)) {
    ab = pitch_type_data$at_bat[i]
    if (pitch_type_data$events[i] == "hit_by_pitch") {
      filter_data <- filter_data %>%
        filter(at_bat != ab)
    }
  }
  
  ## Step 5: Set Outcome Probabilities for Each Count
  zerozero <- filter_data %>%
    filter(count == "0-0") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  zeroone <- filter_data %>%
    filter(count == "0-1") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  onezero <- filter_data %>%
    filter(count == "1-0") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  zerotwo <- filter_data %>%
    filter(count == "0-2") %>%
    summarize(n(), sum(strikeout), sum(ball), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           ball = 'sum(ball)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           ball = ball / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  oneone <- filter_data %>%
    filter(count == "1-1") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  twozero <- filter_data %>%
    filter(count == "2-0") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  onetwo <- filter_data %>%
    filter(count == "1-2") %>%
    summarize(n(), sum(strikeout), sum(ball), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           ball = 'sum(ball)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           ball = ball / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  twoone <- filter_data %>%
    filter(count == "2-1") %>%
    summarize(n(), sum(strike), sum(ball), sum(single), sum(double), 
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           ball = 'sum(ball)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           ball = ball / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  threezero <- filter_data %>%
    filter(count == "3-0") %>%
    summarize(n(), sum(strike), sum(walk), sum(single), sum(double),
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           walk = 'sum(walk)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           walk = walk / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  twotwo <- filter_data %>%
    filter(count == "2-2") %>%
    summarize(n(), sum(strikeout), sum(ball), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           ball = 'sum(ball)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           ball = ball / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  threeone <- filter_data %>%
    filter(count == "3-1") %>%
    summarize(n(), sum(strike), sum(walk), sum(single), sum(double),
              sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strike = 'sum(strike)',
           walk = 'sum(walk)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strike = strike / total,
           walk = walk / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  threetwo <- filter_data %>%
    filter(count == "3-2") %>%
    summarize(n(), sum(strikeout), sum(walk), sum(foul), sum(single), 
              sum(double), sum(triple), sum(homerun), sum(oobip)) %>%
    rename(total = 'n()',
           strikeout = 'sum(strikeout)',
           walk = 'sum(walk)',
           foul = 'sum(foul)',
           single = 'sum(single)',
           double = 'sum(double)',
           triple = 'sum(triple)',
           homerun = 'sum(homerun)',
           oobip = 'sum(oobip)') %>%
    mutate(strikeout = strikeout / total,
           walk = walk / total,
           foul = foul / total,
           single = single / total,
           double = double / total,
           triple = triple / total,
           homerun = homerun / total,
           oobip = oobip / total)
  
  ## Step 6: Build Transition Matrix
  outcome00 <- c(0, zerozero$strike[1], zerozero$ball[1], 0, 0, 0, 0, 0, 0,
                 0, 0, 0, zerozero$single[1], zerozero$double[1], 
                 zerozero$triple[1], zerozero$homerun[1], zerozero$oobip[1], 
                 0, 0)
  
  outcome01 <- c(0, 0, 0, zeroone$strike[1], zeroone$ball[1], 0, 0, 0, 0, 0,
                 0, 0, zeroone$single[1], zeroone$double[1], 
                 zeroone$triple[1], zeroone$homerun[1], zeroone$oobip[1], 
                 0, 0)
  
  outcome10 <- c(0, 0, 0, 0, onezero$strike[1], onezero$ball[1], 0, 0, 0, 0, 
                 0, 0, onezero$single[1], onezero$double[1], 
                 onezero$triple[1], onezero$homerun[1], onezero$oobip[1], 
                 0, 0)
  
  outcome02 <- c(0, 0, 0, zerotwo$foul[1], 0, 0, zerotwo$ball[1], 0, 0, 0, 
                 0, 0, zerotwo$single[1], zerotwo$double[1], 
                 zerotwo$triple[1], zerotwo$homerun[1], zerotwo$oobip[1], 
                 0, zerotwo$strikeout[1])
  
  outcome11 <- c(0, 0, 0, 0, 0, 0, oneone$strike[1], oneone$ball[1], 0, 0, 
                 0, 0, oneone$single[1], oneone$double[1], oneone$triple[1],
                 oneone$homerun[1], oneone$oobip[1], 0, 0)
  
  outcome20 <- c(0, 0, 0, 0, 0, 0, 0, twozero$strike[1], twozero$ball[1], 0, 
                 0, 0, twozero$single[1], twozero$double[1], 
                 twozero$triple[1], twozero$homerun[1], twozero$oobip[1], 
                 0, 0)
  
  outcome12 <- c(0, 0, 0, 0, 0, 0, onetwo$foul[1], 0, 0, onetwo$ball[1], 0, 
                 0, onetwo$single[1], onetwo$double[1], onetwo$triple[1],
                 onetwo$homerun[1], onetwo$oobip[1], 0, onetwo$strikeout[1])
  
  outcome21 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, twoone$strike[1], 
                 twoone$ball[1], 0, twoone$single[1], twoone$double[1], 
                 twoone$triple[1], twoone$homerun[1], twoone$oobip[1], 0, 0)
  
  outcome30 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, threezero$strike[1], 0,
                 threezero$single[1], threezero$double[1], 
                 threezero$triple[1], threezero$homerun[1], 
                 threezero$oobip[1], threezero$walk[1], 0)
  
  outcome22 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, twotwo$foul[1], 0, 
                 twotwo$ball[1], twotwo$single[1], twotwo$double[1], 
                 twotwo$triple[1], twotwo$homerun[1], twotwo$oobip[1], 
                 0, twotwo$strikeout[1])
  
  outcome31 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, threeone$strike[1],
                 threeone$single[1], threeone$double[1], threeone$triple[1],
                 threeone$homerun[1], threeone$oobip[1], 
                 threeone$walk[1], 0)
  
  outcome32 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, threetwo$foul[1], 
                 threetwo$single[1], threetwo$double[1], threetwo$triple[1],
                 threetwo$homerun[1], threetwo$oobip[1], threetwo$walk[1], 
                 threetwo$strikeout)
  
  outcome1B <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
  outcome2B <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
  outcome3B <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
  outcomeHR <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
  outcomeBP <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
  outcomeKK <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  outcomeBB <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
  transition <- matrix(rbind(outcome00, outcome01, outcome10, outcome02, 
                             outcome11, outcome20, outcome12, outcome21, 
                             outcome30, outcome22, outcome31, outcome32, 
                             outcome1B, outcome2B, outcome3B, outcomeHR, 
                             outcomeBP, outcomeKK, outcomeBB), 
                       nrow = 19,
                       ncol = 19)
  rownames(transition) <- c("0-0", "0-1", "1-0", "0-2", "1-1", "2-0", "1-2",
                            "2-1", "3-0", "2-2", "3-1", "3-2", "1B", "2B", 
                            "3B", "HR", "BIP", "BB", "K")
  colnames(transition) <- c("0-0", "0-1", "1-0", "0-2", "1-1", "2-0", "1-2",
                            "2-1", "3-0", "2-2", "3-1", "3-2", "1B", "2B", 
                            "3B", "HR", "BIP", "BB", "K")
  
  return(transition)
}


# Mixed Matrix ------------------------------------------------------------

# Combine matrices
mixer <- function(batterid, pitcherid){
  
  batter <- battermatrix(batterid)
  pitcher <- pitchermatrix(pitcherid, batterid)
  mix <- (batter + pitcher)/2
  
  return(mix)
  
}

# Give steady state outcomes for each matchup
outcomes <- function(batterid, pitcherid){
  
  transition <- mixer(batterid, pitcherid)
  matrix <- transition %^% 1000
  probs <- matrix[1:12,13:19]
  
  for(i in 1:nrow(probs)){
    for(j in 1:ncol(probs)){
      probs[i,j] = round(probs[i,j], 3)
    }
  }
  
  return(probs)
}

# Steady state for individual batter
batoutcomes <- function(batterid){
  
  transition <- battermatrix(batterid)
  matrix <- transition %^% 1000
  probs <- matrix[1:12,13:19]
  
  for(i in 1:nrow(probs)){
    for(j in 1:ncol(probs)){
      probs[i,j] = round(probs[i,j], 3)
    }
  }
  
  return(probs)
}

# Steady state for individual pitcher
pitchoutcomes <- function(pitcherid, batterid){
  
  transition <- pitchermatrix(pitcherid, batterid)
  matrix <- transition %^% 1000
  probs <- matrix[1:12,13:19]
  
  for(i in 1:nrow(probs)){
    for(j in 1:ncol(probs)){
      probs[i,j] = round(probs[i,j], 3)
    }
  }
  
  return(probs)
}

# Example: 2019 Mike Trout vs. 2019 Justin Verlander
b.matrix <- battermatrix(trout)
p.matrix <- pitchermatrix(verlander, trout)
b.outcomes <- batoutcomes(trout)
p.outcomes <- pitchoutcomes(verlander, trout)
combine.matrix <- mixer(trout, verlander)
combine.outcomes <- outcomes(trout, verlander)

b.matrix
p.matrix
b.outcomes
p.outcomes
combine.matrix
combine.outcomes


# Visualization -----------------------------------------------------------

# Bar graph of probabilities on given count
combine.flipped = t(combine.outcomes)
bardata = as.data.frame(combine.flipped)
setDT(bardata, keep.rownames = "outcome")
bardata$outcome = factor(bardata$outcome, 
                         levels = c("1B", "2B", "3B", "HR", "BIP", "BB", "K"))
batter = "2019 Mike Trout"
pitcher = "2019 Justin Verlander"
count = "0-0"

bar_visualization <- ggplot(data = bardata, 
                            mapping = aes(x = outcome, y = bardata$'0-0')) + 
  geom_bar(stat = "identity", 
           fill = "blue") + 
  geom_text(mapping = aes(label = paste0(as.character(format(round((bardata$'0-0' * 100), 1), 1)), "%")),
            vjust = -1, 
            size = 5) + 
  ylim(min = 0, max = (max(bardata$'0-0') + .025)) + 
  labs(x = "At-Bat Outcome", 
       y = "Probability", 
       title = paste0("Outcome Probabilities of an At-Bat between ", batter, 
                 " & ", pitcher), 
       subtitle = paste0("Starting on a ", count, " Count")) + 
  theme_economist_white()

bar_visualization
