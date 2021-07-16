#Zanmi Kafe R-Script:
#Hello person working on this. The three lines bellow are the packages that We used for this sheet. Run these three lines of code, if they don't work then type in install.packages and put in the name of the pacages in that function. After that, run these again. 
library (dplyr)
library(googlesheets4)
library (tidyr)
############################################################################################
#READING IN THE DATA:
# This is a for loop that reads in the data from a Google sheet and puts it into R. You have to do this for loop for each spreadsheet used. The 2019 data is annotates for where values need to be changed for inputting data in later years.   
#-------------------------------------------------------------------------------------------
#For loop and data editing for 2019 data:
data_list <- list()
#For the code on the line bellow, use the numbers to indicate which Google sheets are read in. Our data for the 2019 sheet had household one start on sheet 5 and end on sheet 54. 
sheet_numbers <- 5:54
for(i in 1:length(sheet_numbers)){
  sheet_id <- sheet_numbers[i]
  #This is the place where we entered the Google sheet link to the 2019 data. 
  temp <- read_sheet('https://docs.google.com/spreadsheets/d/1QMTqhd-llPQH_2AfaiV6klHXpe1alPE7Eo85UU82sP0/edit?usp=sharing', sheet = sheet_id)
  #The code on the line below indicates which columns you want to read in. For the 2019 data we wanted to read in the first six columns on the each household spreadsheet. Make sure that the names of the columns are the same on all Google spreadsheets used. You will run into issues if the names or orders of columns are different. 
  temp <- temp[,1:6]
  data_list[[i]] <- temp
}
#This binds the lists into one data frame (we are calling it 'dat')
dat <- do.call('rbind', data_list)
nineteendat <- dat
#This removes the N/A from 'dat'. 
nineteendat <- nineteendat %>% drop_na()
nrow(nineteendat)
nineteendat <- nineteendat[complete.cases(nineteendat),]

nineteendat$year <- 2019

# Repeat these steps to read in and bind 2021 dataset
#Entered in Google sheet link to the 2021 data
data_listtwentyone <- list ()
sheet_numbers <- 1:50
for(i in 1:length(sheet_numbers)){
  sheet_id <- sheet_numbers[i]
  message('fonna read ', sheet_id)
  temptwentyone <- read_sheet ('https://docs.google.com/spreadsheets/d/10AIlsE5MR-0I7kzj__s2g6uOrjIvRm3pixnQZ2-FB4c/edit?usp=sharing', sheet = sheet_id)
  temptwentyone <- temptwentyone [,1:5]
  data_listtwentyone [[i]] <- temptwentyone
}
# # see if household is duplicated in any data frame
# for(i in 1:length(data_listtwentyone)){
#     this_data <- data_listtwentyone[[i]]
#     the_names <- names(this_data)
#     h_names <- grepl('Household', the_names)
#     if(length(which(h_names)) > 1){
#         print(i)
#     }
# }
brew_list <- list()
for(i in 1:length(data_listtwentyone)){
  this_data <- data_listtwentyone[[i]]
  if(is.list(this_data$Height_cm)){
    this_data$Height_cm <- as.numeric(gsub('cm', '', this_data$Height_cm))
  }
  if(is.list(this_data$Diam_mm)){
    this_data$Diam_mm <- as.numeric(gsub('cm|mm', '', this_data$Diam_mm))
  }
  names(this_data) <- gsub('Household number|Household_number', 'Household', names(this_data))
  brew_list[[i]] <- this_data
}
twentyonedat <- bind_rows(brew_list)
twentyonedat <- twentyonedat %>% select (-Diam_mm)
twentyonedat <- twentyonedat %>%
  rename (id = Tree, 
          diam_cm = Diam_corr_cm)
twentyonedat <- twentyonedat %>%
  mutate (year = 2021,
          Height_m = Height_cm / 100) 
twentyonedat <- twentyonedat %>%
  select (-Height_cm)
library(readr)
# this is where we ran to! stop here and examine before binding datasets
#This is code that only applies to the 2019 data. This for loop adds a household ID column to the data frame. 
houses <- unique(nineteendat$Household) ; houses
i=1 # define i to help build for loop
ids <- c() # stage an empty vector for placing id's
for(i in 1:length(houses)){ # begin for loop
  housi <- houses[i] ; housi # determine the house corresponding to this value of i
  treesi <- nineteendat[nineteendat$Household == housi,] ; treesi # subset data to only entries for this house
  idi <- 1:nrow(treesi) ; idi # create a list of ids for trees belonging to this house
  ids <- c(ids, idi) # add these ids to the growing list of ids
}
ids
nineteendat$id <- ids # add these id's as a column to the data frame
head(dat) # check it out
#This changes the names of the variables into names that can easily be typed into R functions and Dplyr inputs. We keep the variables Household and Species the same but change the heights and diameters with units. This also takes out columns that we do not need. 
nineteendat <- nineteendat %>% 
  select(Household, Species, Height_m = 'H (m)', diam_cm = 'dbh (cm)' )


################### Combine 2019 and 2021 data
dat <-bind_rows(nineteendat, twentyonedat)
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
#-------------------------------------------------------------------------------------------
#For loop and data editing for 2021 data:
############################################################################################
#EQUATIONS
#The first three equations are ones that Dr. McGrath found. For each equation, the last name of the author in the paper the equations was found is listed first, and the name of the tree the equation goes with is listed second. 
#-------------------------------------------------------------------------------------------
#McGrath Eeuations: 
# This is the green equation on Dr. McGrath's spreadsheet.  
cole_ewel <- function(d,h){
  biomass <- 1.631+.017*(d^2)*h
  carbon <- biomass * 0.5
  CO2equ <- carbon * 3.6663
  CO2equ_tons <- CO2equ *0.001102
  return(CO2equ_tons)
}
# This is the code that creates a new column in the data with the equation from the cole&ewel paper (green equation). 
# dat <- dat %>%
# mutate(cole_ewel = cole_ewel(diam_cm, Height_m))
# 
# #I am going to take this out in favor for a more specific formula:
# dat <- dat %>%
#   select(-cole_ewel)
#This is the blue equation on Dr. McGrath's spreadsheet.
chave <- function(d,h){
  Abovebiomass <- exp(-2.187) * (0.42*(d^2)*h)^0.916
  ABBiomass <- Abovebiomass * 1.2
  carbon <- ABBiomass * 0.5
  CO2equ <- carbon *3.6663
  return(CO2equ)
}
# This is the code that creates a new column in the data with the equation from the chave paper (blue equation). 
# dat <- dat %>%
#   mutate(chave = chave(diam_cm, Height_m))
# 
# #I am going to take this out in favor for a more specific formula:
# 
# dat <- dat %>% 
#   select(-chave)
#This is the orange equation on Dr. McGrath's spreadsheet.
TFTF <- function(d,h){
  Abovebiomass <- 0.25*((d/2.54)^2)*(h*3.28)
  Mass <- Abovebiomass/2.205
  ABBiomass <- Mass*1.2
  carbon <- ABBiomass * 0.5
  CO2equ <- carbon *3.6663
  return(CO2equ)
}
# 
# # This is the code that creates a new column in the data with the equation from the TFTF paper (orange equation). 
# 
# dat <- dat %>%
#   mutate(TFTF = TFTF(diam_cm, Height_m))
#I am going to take this out in favor for a more specific formula:
# dat <- dat %>% 
#   select(-TFTF)
#-------------------------------------------------------------------------------------------
#Equations that we found 
#This is our found equation for Mango. The final answer is given in tons. INCLUDES ABOVE AND BELOW GROUND BIOMASS!
Sharma_Mango <- function(d){
  Abovebiomass <- 34.4703 - 8.067*(d) + 0.6589*(d^2)
  BelowGroundBiomass <- Abovebiomass*1.2
  TotalBiomass <- BelowGroundBiomass + Abovebiomass
  carbon <- TotalBiomass * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}
# # This is the code that creates a new column in the data with the equation from the Sharma et al. paper that gives us the CO2 equ in tons for mango. 
# dat <- dat %>%
#   mutate(Mango = Sharma_Mango(diam_cm))
# 
# dat <- dat %>%
#   select(-Mango)
#This is the equation for the Mahogany tree. One thing to note about this equation is that it only calculates for above ground biomass and does not include below ground biomass. Ask dr. McGrath if we need to have both.
#INCLUDES ONLY ABOVE GROUND BIOMASS
Dickert_Mahogany <- function(d,h){
  Abovebiomass <- 0.09029*((d^2)*h)^(0.684)
  BlowGround <- Abovebiomass *1.2
  AGABG <- Abovebiomass + BlowGround
  carbon <- AGABG * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}
# This is the code that creates a new column in the data with the equation from the Dickert paper. 
# dat <- dat %>%
#   mutate(Mahogany = Dickert_Mahogany(diam_cm, Height_m))
# 
# dat <- dat %>%
#   select(-Mahogany)
# I think this is the equation for just the above ground biomass. This is the cedrela tree equation from the Cole and Ewel paper. INLCUDES ONLY THE ABOVE GROUND BIOMASS!
Cole_Cedrela <- function(d,h){
  Abovebiomass <- 0.0448*((d^2)*h)^(0.4879)
  BlowGround <- Abovebiomass *1.2
  AGABG <- Abovebiomass + BlowGround
  carbon <- AGABG * 0.5
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
  BelowBiomass <- carbonTree*1.2
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
#-------------------------------------------------------------------------------------------
#BINDING MULTIPAL EQUATIONS TOGETHER TO CREATE A NEW COLUMN FOR THE DATA TAB ON THE DASHBOARD. 
# This is the code that binds all the equations together into one column based off of the tree species. 
dat <- dat %>% 
  select(Household, Species, diam_cm, Height_m, id, year) %>% 
  mutate(Calculations = case_when(Species == 'M' ~ Sharma_Mango(diam_cm),
                                  Species == 'A' ~ Dickert_Mahogany(diam_cm, Height_m),
                                  Species == 'C' ~ Cole_Cedrela(diam_cm, Height_m),
                                  Species == 'K' ~ Padjung_Coffee(diam_cm),
                                  Species == 'KK' ~ Padjung_Coffee(diam_cm)
  )
  )
############################################################################################
#READ THIS ALL INTO A CSV FILE TO PASS TO THE SHINEY DASHBOARD R SCRIPT
# This is some code that was used to help with our shiney app
#
write_csv(dat, 'dat.csv')
