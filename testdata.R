library (dplyr)
library(googlesheets4)
library (tidyr)

# This converts each speadsheet into a dataframe and compules a list. 

data_list <- list()
sheet_numbers <- 5:54
for(i in 1:length(sheet_numbers)){
  sheet_id <- sheet_numbers[i]
  temp <- read_sheet('https://docs.google.com/spreadsheets/d/1QMTqhd-llPQH_2AfaiV6klHXpe1alPE7Eo85UU82sP0/edit?usp=sharing', sheet = sheet_id)
  temp <- temp[,1:6]
  data_list[[i]] <- temp
}
dat <- do.call('rbind', data_list)

dat <- dat %>% drop_na()
# Data now ready for app, no NAs


dat <- dat %>%
  mutate(Species = case_when(
    Species == 'Akajou'~ 'A',
    Species == 'A' ~ 'A',
    Species == 'mango' ~ 'M',
    Species == 'M' ~ 'M',
    Species == 'Kafe' ~ 'K',
    Species == 'K' ~ 'K',
    Species == 'Ced' ~ 'C',
    Species == 'C' ~ 'C',
    Species == 'New Kafe' ~ 'KK',
    Species == 'KK' ~ 'KK'
  ))

dat <- dat %>% 
  select(Household, Species, Height_m = 'H (m)', Height_cm = 'Height (cm)', diam_mm = 'diam (mm)', diam_cm = 'dbh (cm)' )

cole_ewel <- function(d,h){
  biomass <- 1.631+.017*(d^2)*h
  carbon <- biomass * .5
  CO2equ <- carbon * 3.6663
  return(CO2equ)
}


dat <- dat %>% 
  mutate(cole_ewel = cole_ewel(diam_cm, Height_m))

chave <- function(d,h){
  Abovebiomass <- exp(-2.187) * (.42*(d^2)*h)^.916
   ABBiomass <- Abovebiomass * 1.2
   carbon <- ABBiomass * .5
   CO2equ <- carbon *3.6663
  return(CO2equ)
}

dat <- dat %>% 
  mutate(chave = chave(diam_cm, Height_m))

TFTF <- function(d,h){
  Abovebiomass <- .25*((d/2.54)^2)*(h*3.28)
  Mass <- Abovebiomass/2.205
  ABBiomass <- Mass*1.2
  carbon <- ABBiomass * .5
  CO2equ <- carbon *3.6663
  return(CO2equ)
}

dat <- dat %>% 
  mutate(TFTF = TFTF(diam_cm, Height_m))
