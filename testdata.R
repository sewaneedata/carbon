library (dplyr)
library(googlesheets4)
library (tidyr)

# This converts each spreadsheet into a data frame and compiles a list. 

data_list <- list()
sheet_numbers <- 5:54
for(i in 1:length(sheet_numbers)){
  sheet_id <- sheet_numbers[i]
  temp <- read_sheet('https://docs.google.com/spreadsheets/d/1QMTqhd-llPQH_2AfaiV6klHXpe1alPE7Eo85UU82sP0/edit?usp=sharing', sheet = sheet_id)
  temp <- temp[,1:6]
  data_list[[i]] <- temp
}

#This binds the lists into one data frame (we are calling it 'dat')

dat <- do.call('rbind', data_list)


#This removes the N/A from 'dat'. 

dat <- dat %>% drop_na()


#In the species column, this makes names of the plants uniform. All of the A's are Akajou, and the M's are Mango, all the K's are Kafe, all the C's are Ced, and all the KK's are New Kafe. 

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


#This changes the names of the variables into names that can easily be typed into R functions and Dplyr inputs. We keep the variables Household and Species the same but change the heights and diameters with units. 

dat <- dat %>% 
  select(Household, Species, Height_m = 'H (m)', Height_cm = 'Height (cm)', diam_mm = 'diam (mm)', diam_cm = 'dbh (cm)' )


# This is the green equation on Dr. McGrath's spreadsheet.  

cole_ewel <- function(d,h){
  biomass <- 1.631+.017*(d^2)*h
  carbon <- biomass * .5
  CO2equ <- carbon * 3.6663
  return(CO2equ)
}

# This is the code that creates a new column in the data with the equation from the cole&ewel paper (green equation). 
dat <- dat %>% 
  mutate(cole_ewel = cole_ewel(diam_cm, Height_m))


#This is the blue equation on Dr. McGrath's spreadsheet.

chave <- function(d,h){
  Abovebiomass <- exp(-2.187) * (.42*(d^2)*h)^.916
   ABBiomass <- Abovebiomass * 1.2
   carbon <- ABBiomass * .5
   CO2equ <- carbon *3.6663
  return(CO2equ)
}

# This is the code that creates a new column in the data with the equation from the chave paper (blue equation). 

dat <- dat %>% 
  mutate(chave = chave(diam_cm, Height_m))


#This is the orange equation on Dr. McGrath's spreadsheet.

TFTF <- function(d,h){
  Abovebiomass <- .25*((d/2.54)^2)*(h*3.28)
  Mass <- Abovebiomass/2.205
  ABBiomass <- Mass*1.2
  carbon <- ABBiomass * .5
  CO2equ <- carbon *3.6663
  return(CO2equ)
}

# This is the code that creates a new column in the data with the equation from the TFTF paper (orange equation). 

dat <- dat %>% 
  mutate(TFTF = TFTF(diam_cm, Height_m))
