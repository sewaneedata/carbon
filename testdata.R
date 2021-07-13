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

# Taking out columns that we don't need. 
dat <- dat %>%
  select(-Height_cm)
dat <- dat %>%
  select(-diam_mm)

# This is the green equation on Dr. McGrath's spreadsheet.  

#cole_ewel <- function(d,h){
 # biomass <- 1.631+.017*(d^2)*h
  #carbon <- biomass * 0.5
  #CO2equ <- carbon * 3.6663
  #return(CO2equ)
#}

# This is the code that creates a new column in the data with the equation from the cole&ewel paper (green equation). 
#dat <- dat %>% 
  #mutate(cole_ewel = cole_ewel(diam_cm, Height_m))

#I am going to take this out in favor for a more specific formula:
# dat <- dat %>% 
#   select(-cole_ewel)


#This is the blue equation on Dr. McGrath's spreadsheet.

# chave <- function(d,h){
#   Abovebiomass <- exp(-2.187) * (0.42*(d^2)*h)^0.916
#    ABBiomass <- Abovebiomass * 1.2
#    carbon <- ABBiomass * 0.5
#    CO2equ <- carbon *3.6663
#   return(CO2equ)
# }

# This is the code that creates a new column in the data with the equation from the chave paper (blue equation). 

# dat <- dat %>% 
#   mutate(chave = chave(diam_cm, Height_m))
# 
# #I am going to take this out in favor for a more specific formula:
# 
# dat <- dat %>% 
#   select(-chave)


#This is the orange equation on Dr. McGrath's spreadsheet.

# TFTF <- function(d,h){
#   Abovebiomass <- 0.25*((d/2.54)^2)*(h*3.28)
#   Mass <- Abovebiomass/2.205
#   ABBiomass <- Mass*1.2
#   carbon <- ABBiomass * 0.5
#   CO2equ <- carbon *3.6663
#   return(CO2equ)
# }
# 
# # This is the code that creates a new column in the data with the equation from the TFTF paper (orange equation). 
# 
# dat <- dat %>% 
#   mutate(TFTF = TFTF(diam_cm, Height_m))

#I am going to take this out in favor for a more specific formula:

# dat <- dat %>% 
#   select(-TFTF)



#This is our found equation for Mango. The final answer is given in tons. INCLUDES ABOVE AND BELOW GROUND BIOMASS!

Sharma_Mango <- function(d){
  Abovebiomass <- 34.4703 - 8.067*(d) + 0.6589*(d^2)
  BelowGroundBiomass <- Abovebiomass*0.15
  TotalBiomass <- BelowGroundBiomass + Abovebiomass
  carbon <- TotalBiomass * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}

# # This is the code that creates a new column in the data with the equation from the Sharma et al. paper that gives us the CO2 equ in tons for mango. 
# dat <- dat %>% 
#   mutate(Mango = Sharma_Mango(diam_cm))
# dat <- dat %>%
#   select(-Mango)


#This is the equation for the Mahogany tree. One thing to note about this equation is that it only calculates for above ground biomass and does not include below ground biomass. Ask dr. McGrath if we need to have both.
#INCLUDES ONLY ABOVE GROUND BIOMASS

Dickert_Mahogany <- function(d,h){
  Abovebiomass <- 0.09029*((d^2)*h)^(0.684)
  carbon <- Abovebiomass * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}


# This is the code that creates a new column in the data with the equation from the Dickert paper. 

# dat <- dat %>% 
#   mutate(Mahogany = Dickert_Mahogany(diam_cm, Height_m))
# dat <- dat %>%
#   select(-Mahogany)

# I think this is the equation for just the above ground biomass. This is the cedrela tree equation from the Cole and Ewel paper. INLCUDES ONLY THE ABOVE GROUND BIOMASS!

Cole_Cedrela <- function(d,h){
  Abovebiomass <- 0.0448*((d^2)*h)^(0.4879)
  carbon <- Abovebiomass * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}

# This is the code that creates a new column in the data with the equation from the Cole and Ewel paper. 

# dat <- dat %>% 
#   mutate(Cedrela = Cole_Cedrela(diam_cm, Height_m))
# dat <- dat %>%
#   select(-Cedrela)

# This is the equation from the Padjung paper. This includes the trees but not the coffee beans or harvested coffee. This calculates the above ground and below ground biomass of the coffee tree. 
# INCLUDES ABOVE AND BELOW GROUND BIOMASS!

Padjung_Coffee <- function(d){
  Abovebiomass <- 0.11*0.62*(d)*2.62
  carbonTree <- Abovebiomass * 0.5
  BelowBiomass <- carbonTree*0.2
  AGABGBiomass <- BelowBiomass + carbonTree
  CO2equ_kg <- AGABGBiomass *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}

# This is the code that creates a new column in the data with the equation from the Cole and Padjung paper. 
# dat <- dat %>% 
#   mutate(Coffee = Padjung_Coffee(diam_cm))
# dat <- dat %>%
#   select(-Coffee)


# This is the code that binds all the equations together into one column based off of the tree species. 
dat <- dat %>% 
  select(Household, Species, diam_cm, Height_m) %>% 
  mutate(Calculations = case_when(Species == 'M' ~ Sharma_Mango(diam_cm),
                                  Species == 'A' ~ Dickert_Mahogany(diam_cm, Height_m),
                                  Species == 'C' ~ Cole_Cedrela(diam_cm, Height_m),
                                  Species == 'K' ~ Padjung_Coffee(diam_cm),
                                  Species == 'KK' ~ Padjung_Coffee(diam_cm)
                                  )
  )







#GGPLOT STUFF FOR SHINYAPP

# This is some code that was used to help with our shiney app
write.csv(dat, '~/Documents/datascience/carbon/dat.csv')


library(reshape2)
library(ggplot2)
pd <- dat %>% select(Household, Species, cole_ewel, chave, TFTF)
pd <- melt(pd, id.vars = c('Household', 'Species'))

ggplot(pd, aes(x = variable, y =value, fill = Species )) + geom_bar(stat = 'identity', position = 'dodge')
