#Zanmi Kafe R-Script:
#Hello person working on this. The lines bellow are the packages that We used for this sheet. Run these three lines of code, if they don't work then type in install.packages and put in the name of the packages in that function. After that, run these again. 
library (dplyr)
library(googlesheets4)
library (tidyr)
library(readr)
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

# This makes this data frame into one that is designated for the 2019 data. 

nineteendat <- dat

#This removes the N/A from 'dat'. 
nineteendat <- nineteendat %>% drop_na()
nrow(nineteendat)
nineteendat <- nineteendat[complete.cases(nineteendat),]

#This puts a year column in the 2019 data and assigns the year for each row as 2019.  
nineteendat$year <- 2019

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
  select(Household, Species, Height_m = 'H (m)', diam_cm = 'dbh (cm)', year, id )


#-------------------------------------------------------------------------------------------
#For loop and data editing for 2021 data:
# Repeat these steps to read in and bind 2021 data set
#Entered in Google sheet link to the 2021 data
data_listtwentyone <- list ()
sheet_numbers <- 1:50
for(i in 1:length(sheet_numbers)){
  sheet_id <- sheet_numbers[i]
  message('fonna read ', sheet_id)
  temptwentyone <- read_sheet ('https://docs.google.com/spreadsheets/d/10AIlsE5MR-0I7kzj__s2g6uOrjIvRm3pixnQZ2-FB4c/edit#gid=1958132654', sheet = sheet_id)
  temptwentyone <- temptwentyone [,1:5]
  data_listtwentyone [[i]] <- temptwentyone
}

#This is something that Joe wrote that changes the names of the columns because the column names for 2021 did not match the column names for 2019. It also takes out the measurements in the data. There was sime data that had cm or mm after the measurement. 
brew_list <- list()
for(i in 1:length(data_listtwentyone)){
  this_data <- data_listtwentyone[[i]]
  if(is.list(this_data$Height_cm)){
    this_data$Height_cm <- as.numeric(gsub('cm', '', this_data$Height_cm))
  }

  if(is.list(this_data$Diam_mm)){
    this_data$Diam_mm <- as.numeric(gsub('cm|mm', '', this_data$Diam_mm))
  }
  if(is.list(this_data$Diam_corr_cm)){
    this_data$Diam_corr_cm <- as.numeric(gsub('cm|mm', '', this_data$Diam_corr_cm))
  }
  names(this_data) <- gsub('Household number|Household_number', 'Household', names(this_data))
  if(is.list(this_data$Household)){
    this_data$Household <- as.numeric(unlist(this_data$Household))
  }
  if(is.list(this_data$Tree)){
    this_data$Tree <- as.numeric(unlist(this_data$Tree))
  }
  brew_list[[i]] <- this_data
}

#This binds the 2021 data into a data frame. 
twentyonedat <- bind_rows(brew_list)
twentyonedat <- twentyonedat %>% select (-Diam_mm)
#This makes sure all the column names are the same. 
twentyonedat <- twentyonedat %>%
  rename (id = Tree, 
          diam_cm = Diam_corr_cm)
twentyonedat <- twentyonedat %>%
  mutate (year = 2021,
          Height_m = Height_cm / 100) 
twentyonedat <- twentyonedat %>%
  select (-Height_cm)

# this is where we ran to! stop here and examine before binding datasets



################### Combine 2019 and 2021 data
dat <-bind_rows(nineteendat, twentyonedat)

dat <- dat %>% mutate(Species = toupper(Species))

#This removes the N/A from 'dat'. 

dat <- dat %>% drop_na()
dat <- dat[complete.cases(dat),]

#This removes stuff from our environment that we don't need. 
rm(brew_list, data_listtwentyone, temptwentyone, this_data, treesi, houses, housi, i, idi, ids, sheet_id, sheet_numbers, temp, data_list)





#In the species column, this makes names of the plants uniform. All of the A's are Akajou, and the M's are Mango, all the K's are Kafe, all the C's are Ced, and all the KK's are New Kafe.
dat <- dat %>%
  mutate(Species = case_when(
    Species == 'AKAJOU'~ 'A',
    Species == 'A' ~ 'A',
    Species == 'MANGO' ~ 'M',
    Species == 'M' ~ 'M',
    Species == 'KAFE' ~ 'K',
    Species == 'K' ~ 'K',
    Species == 'CED' ~ 'C',
    Species == 'C' ~ 'C',
    Species == 'NEW KAFE' ~ 'KK',
    Species == 'KK' ~ 'KK'
  ))

############################################################################################
#EQUATIONS
#The first three equations are ones that Dr. McGrath found. For each equation, the last name of the author in the paper the equations was found is listed first, and the name of the tree the equation goes with is listed second. 
#-------------------------------------------------------------------------------------------
#McGrath Eeuations: 
# This is the green equation on Dr. McGrath's 2019 spreadsheet.  
cole_ewel <- function(d,h){
  biomass <- 1.631+.017*(d^2)*h
  carbon <- biomass * 0.5
  CO2equ <- carbon * 3.6663
  CO2equ_tons <- CO2equ *0.001102
  return(CO2equ_tons)
}


# This is the code that creates a new column in the data with the equation from the cole&ewel paper (green equation). 
dat <- dat %>%
mutate(cole_ewel = cole_ewel(diam_cm, Height_m))
# 
# #I am going to take this out in favor for a more specific formula:
# dat <- dat %>%
#   select(-cole_ewel)

#This is the orange equation on Dr. McGrath's spreadsheet.
TFTF <- function(d,h){
  Abovebiomass <- 0.25*((d/2.54)^2)*(h*3.28)
  Mass <- Abovebiomass/2.205
  ABBiomass <- Mass*1.2
  carbon <- ABBiomass * 0.5
  CO2equ <- carbon *3.6663
  CO2equ_tons <- CO2equ *0.001102
  return(CO2equ_tons)
}

# 
# # This is the code that creates a new column in the data with the equation from the TFTF paper (orange equation). 
# 
dat <- dat %>%
  mutate(TFTF = TFTF(diam_cm, Height_m))
#I am going to take this out in favor for a more specific formula:
# dat <- dat %>% 
#   select(-TFTF)
#-------------------------------------------------------------------------------------------
#Equations that we found 
#This is our found equation for Mango. The final answer is given in tons. 
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

#This is the equation for the Mahogany tree.

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

# This is the cedrela tree equation from the Cole and Ewel paper. 
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


#This equations came from the Mexico paper. 
Acosta_Coffee <- function(d){
  Abovebiomass <- exp(-0.66)*((d)^1.37)
  carbonTree <- Abovebiomass *0.5
  BelowBiomass <- carbonTree*1.2
  AGABGBiomass <- BelowBiomass + carbonTree
  CO2equ_kg <- AGABGBiomass *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}

# This is the code that creates a new column in the data with the equation listed above.
# dat <- dat %>%
#   mutate(Acosta_Coffee = Acosta_Coffee(diam_cm))
# dat <- dat %>%
#   select(-Acosta_Coffee)


#This equations is from the Chave paper and gives a general equation for carbon.
Chave_General <- function(d,h){
  Abovebiomass <- (0.0673)*((.42)*(d^2)*(h))^0.976
  carbonTree <- Abovebiomass *0.5
  BelowBiomass <- carbonTree*1.2
  AGABGBiomass <- BelowBiomass + carbonTree
  CO2equ_kg <- AGABGBiomass *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}
# This is the code that creates a new column in the data with the equation listed above.
dat <- dat %>%
  mutate(Chave_General = Chave_General(diam_cm, Height_m))
# dat <- dat %>%
#   select(-Chave_General)
#-------------------------------------------------------------------------------------------
#BINDING MULTIPAL EQUATIONS TOGETHER TO CREATE A NEW COLUMN FOR THE DATA TAB ON THE DASHBOARD. 
# This is the code that binds all the equations together into one column based off what Dr. McGrath found appropriate. 
dat <- dat %>% 
  # select(Household, Species, diam_cm, Height_m, id, year) %>% 
  mutate(Calculations = case_when(Species == 'M' ~ Sharma_Mango(diam_cm),
                                  Species == 'A' ~ Chave_General(diam_cm, Height_m),
                                  Species == 'C' ~ Chave_General(diam_cm, Height_m),
                                  Species == 'K' ~ Acosta_Coffee(diam_cm),
                                  Species == 'KK' ~ Acosta_Coffee(diam_cm)
  )
  )

#This is what we used for the Allometric equations tab to look at each equations that are speciefic for each tree. 
dat <- dat %>% 
  # select(Household, Species, diam_cm, Height_m, id, year) %>% 
  mutate(Species_Specific = case_when(Species == 'M' ~ Sharma_Mango(diam_cm),
                                  Species == 'A' ~ Dickert_Mahogany(diam_cm, Height_m),
                                  Species == 'C' ~ Cole_Cedrela(diam_cm, Height_m),
                                  Species == 'K' ~ Acosta_Coffee(diam_cm),
                                  Species == 'KK' ~ Acosta_Coffee(diam_cm)
  )
  )

#This line of code is for fraud detection. Information sent to Dr. McGrath in an email. 
# fix <- dat%>%
#   mutate(LogHeight = log(Height_m))%>%
#   filter(LogHeight <= -2.5)
# 
# table(fix$Household)
############################################################################################
#READ THIS ALL INTO A CSV FILE TO PASS TO THE SHINEY DASHBOARD R SCRIPT


write_csv(dat, 'dat.csv')


