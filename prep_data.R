#Zanmi Kafe R-Script:
#Hello person working on this. The lines bellow are the packages that We used for this sheet. Run these three lines of code, if they don't work then type in install.packages and put in the name of the packages in that function. After that, run these again.
library (dplyr)
library(googlesheets4)
library (tidyr)
library(readr)
#loads the equations that are held in the functions files
source('functions.R')

############################################################################################
# SWITCH DEBUG MODE ON or OFF
# This code toggles 'debugging' mode on (if you are running this code outside of the app)
# of off (default -- if this code is ready to be sourced the app)

debug_mode <- TRUE


############################################################################################
#READING IN THE DATA:
# This is a for loop that reads in the data from a Google sheet and puts it into R. You have to do this for loop for each spreadsheet used. The 2019 data is annotates for where values need to be changed for inputting data in later years.


#-------------------------------------------------------------------------------------------
#For loop and data editing for 2019 data:
data_list <- list()
#For the code on the line bellow, use the numbers to indicate which Google sheets are read in. Our data for the 2019 sheet had household one start on sheet 5 and end on sheet 54.
sheet_numbers <- 5:54
i=2
for(i in 1:length(sheet_numbers)){
  # Increment the progress bar, and update the detail text.
  if(debug_mode == FALSE){incProgress(1/115, detail = paste("Reading 2019 data ..."))}

  sheet_id <- sheet_numbers[i]
  #This is the place where we entered the Google sheet link to the 2019 data.
  gs4_deauth()
  temp <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1QMTqhd-llPQH_2AfaiV6klHXpe1alPE7Eo85UU82sP0/edit?usp=sharing',
                             sheet=sheet_id)
  # The code on the line below indicates which columns you want to read in.
  # For the 2019 data we wanted to read in the first six columns on the each household spreadsheet.
  # Make sure that the names of the columns are the same on all Google spreadsheets used.
  # You will run into issues if the names or orders of columns are different.
  temp <- temp[,1:6]
  #head(temp)
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
  dplyr::select(Household, Species, Height_m = 'H (m)', diam_cm = 'dbh (cm)', year, id )


#-------------------------------------------------------------------------------------------
#For loop and data editing for 2021 data:
# Repeat these steps to read in and bind 2021 data set
#Entered in Google sheet link to the 2021 data
data_listtwentyone <- list ()
sheet_numbers <- 1:50
for(i in 1:length(sheet_numbers)){
  # Increment the progress bar, and update the detail text.
  if(debug_mode == FALSE){incProgress(1/115, detail = paste("Reading 2020 data ..."))}

  sheet_id <- sheet_numbers[i]
  message('fonna read ', sheet_id)
  gs4_deauth()
  temptwentyone <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/10AIlsE5MR-0I7kzj__s2g6uOrjIvRm3pixnQZ2-FB4c/edit#gid=1958132654',
                                    sheet=sheet_id)
  temptwentyone <- temptwentyone [,1:5]
  head(temptwentyone)
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

# Increment the progress bar, and update the detail text.
if(debug_mode == FALSE){incProgress(1/115, detail = paste("Formatting data..."))}

#This binds the 2021 data into a data frame.
twentyonedat <- bind_rows(brew_list)
head(twentyonedat)
#twentyonedat <- twentyonedat %>% select (-Diam_mm)
#This makes sure all the column names are the same.
twentyonedat <- twentyonedat %>%
  dplyr::rename (id = Tree,
          diam_cm = Diam_corr_cm)
twentyonedat <- twentyonedat %>%
  dplyr::mutate (year = 2021,
          Height_m = Height_cm / 100)
twentyonedat <- twentyonedat %>%
  dplyr::select (-Height_cm)

# this is where we ran to! stop here and examine before binding datasets

head(nineteendat)
head(twentyonedat)

#making the same order of the columns so no errors occur and the data can be bound together
nineteendat <- nineteendat[,order(names(nineteendat))]
twentyonedat <- twentyonedat[,order(names(twentyonedat))]

################### Combine 2019 and 2021 data
dat <- rbind(nineteendat, twentyonedat)
# dat <-bind_rows(nineteendat, twentyonedat)

dat <- dat %>%
  dplyr::mutate(Species = toupper(Species))

#This removes the N/A from 'dat'.

dat <- dat %>% drop_na()
dat <- dat[complete.cases(dat),]

#This removes stuff from our environment that we don't need.
rm(brew_list, data_listtwentyone, temptwentyone, this_data, treesi, houses, housi, i, idi, ids, sheet_id, sheet_numbers, temp, data_list)


dat <- dat%>%
  dplyr::select(household = Household, species = Species, height_m = Height_m, diam_cm, year, id)


#In the species column, this makes names of the plants uniform. All of the A's are Akajou, and the M's are Mango, all the K's are Kafe, all the C's are Ced, and all the KK's are New Kafe.
head(dat)
table(dat$species)

dat <- dat %>%
  dplyr::mutate(species = case_when(
    species == 'AKAJOU'~ 'A',
    species == 'A' ~ 'A',
    species == 'MANGO' ~ 'M',
    species == 'M' ~ 'M',
    species == 'KAFE' ~ 'K',
    species == 'K' ~ 'K',
    species == 'CED' ~ 'C',
    species == 'C' ~ 'C',
    species == 'NEW KAFE' ~ 'KK',
    species == 'KK' ~ 'KK'
  ))

############################################################################################
#EQUATIONS
#The first three equations are ones that Dr. McGrath found. For each equation, the last name of the author in the paper the equations was found is listed first, and the name of the tree the equation goes with is listed second.
#-------------------------------------------------------------------------------------------
#McGrath Eeuations:

# This is the code that creates a new column in the data with the equation from the cole&ewel paper (green equation).
dat <- dat %>%
  dplyr::mutate(cole_ewel = cole_ewel(as.numeric(diam_cm), as.numeric(height_m)))
#
# #I am going to take this out in favor for a more specific formula:
# dat <- dat %>%
#   select(-cole_ewel)

#
# # This is the code that creates a new column in the data with the equation from the TFTF paper (orange equation).
#
dat <- dat %>%
  dplyr::mutate(TFTF = TFTF(as.numeric(diam_cm), as.numeric(height_m)))
#I am going to take this out in favor for a more specific formula:
# dat <- dat %>%
#   select(-TFTF)
#-------------------------------------------------------------------------------------------

# # This is the code that creates a new column in the data with the equation from the Sharma et al. paper that gives us the CO2 equ in tons for mango.
# dat <- dat %>%
#   mutate(Mango = Sharma_Mango(diam_cm))
#
# dat <- dat %>%
#   select(-Mango)


# This is the code that creates a new column in the data with the equation from the Dickert paper.
# dat <- dat %>%
#   mutate(Mahogany = Dickert_Mahogany(diam_cm, height_m))
#
# dat <- dat %>%
#   select(-Mahogany)


# This is the code that creates a new column in the data with the equation from the Cole and Ewel paper.
# dat <- dat %>%
#   mutate(Cedrela = Cole_Cedrela(diam_cm, height_m))
# dat <- dat %>%
#   select(-Cedrela)


# This is the code that creates a new column in the data with the equation listed above.
# dat <- dat %>%
#   mutate(Acosta_Coffee = Acosta_Coffee(diam_cm))
# dat <- dat %>%
#   select(-Acosta_Coffee)

# This is the code that creates a new column in the data with the equation listed above.
dat <- dat %>%
  dplyr::mutate(Chave_General = Chave_General(as.numeric(diam_cm), as.numeric(height_m)))

# dat <- dat %>%
#   select(-Chave_General)

#-------------------------------------------------------------------------------------------
#BINDING MULTIPAL EQUATIONS TOGETHER TO CREATE A NEW COLUMN FOR THE DATA TAB ON THE DASHBOARD.
# This is the code that binds all the equations together into one column based off what Dr. McGrath found appropriate.
dat <- dat %>%
  # select(Household, Species, diam_cm, Height_m, id, year) %>%
  dplyr::mutate(calculations = case_when(species == 'M' ~ Sharma_Mango(as.numeric(diam_cm)),
                                  species == 'A' ~ Chave_General(as.numeric(diam_cm),
                                                                 as.numeric(height_m)),
                                  species == 'C' ~ Chave_General(as.numeric(diam_cm), as.numeric(height_m)),
                                  species == 'K' ~ Acosta_Coffee(as.numeric(diam_cm)),
                                  species == 'KK' ~ Acosta_Coffee(as.numeric(diam_cm))
  )
  )

#This is what we used for the Allometric equations tab to look at each equations that are speciefic for each tree.
dat <- dat %>%
  # select(Household, Species, diam_cm, Height_m, id, year) %>%
  dplyr::mutate(species_specific = case_when(species == 'M' ~ Sharma_Mango(as.numeric(diam_cm)),
                                  species == 'A' ~ Dickert_Mahogany(as.numeric(diam_cm), as.numeric(height_m)),
                                  species == 'C' ~ Cole_Cedrela(as.numeric(diam_cm), as.numeric(height_m)),
                                  species == 'K' ~ Acosta_Coffee(as.numeric(diam_cm)),
                                  species == 'KK' ~ Acosta_Coffee(as.numeric(diam_cm))
  )
  )

#This line of code is for fraud detection. Information sent to Dr. McGrath in an email.
# fix <- dat%>%
#   mutate(LogHeight = log(height_m))%>%
#   filter(LogHeight <= -2.5)
#
# table(fix$household)

############################################################################################
# Store this dataset in a CSV -- usable in Shiny and on its own, for analyses etc.

# Check it out
head(dat)
unique(dat$household)
table(dat$year,dat$household)

#plot(cole_ewel ~ diam_cm,data=dat[dat$year==2021,])

# Store it
write_csv(dat, 'dat.csv')

############################################################################################
# Formatting this CSV-ready file into an R object ready for use in Shiny

#This changes the names of the trees from just a letter to their full names.
# It is important to run this before working with the code below.
dat <- dat %>%
  dplyr::mutate(species = case_when(
    species == 'A' ~ 'Akajou',
    species == 'M' ~ 'Mango',
    species == 'K' ~ 'Kafe',
    species == 'C' ~ 'Ced',
    species == 'KK' ~ 'New Kafe'
  ))

# This code makes the year 2021 the difference between 2019 and 2021
# so we can look at change over time.

sub2019 <- dat %>%
  dplyr::filter(year==2019) %>%
  dplyr::group_by(household, species) %>%
  dplyr::summarize(Number=n(), Total=sum(calculations))

sub2021 <- dat %>%
  dplyr::filter(year==2021) %>%
  dplyr::group_by(household, species) %>%
  dplyr::summarize(Number=n(), Total=sum(calculations))

joined_19_21 <- inner_join( sub2019,
                            sub2021,
                            by = c("household", "species"))

joined_19_21 <- joined_19_21 %>%
  dplyr::mutate( sum_tree = Number.y - Number.x,
          calculations = Total.y - Total.x)

joined_19_21 <- joined_19_21 %>%
  dplyr::select(-Number.x, -Total.x,
         -Number.y, -Total.y)

joined_19_21 <- joined_19_21 %>%
  dplyr::mutate(species = case_when(
    species == 'Akajou'~ 'Akajou',
    species == 'Mango' ~ 'Mango',
    species == 'Kafe' ~ 'Kafe',
    species == 'Ced' ~ 'Ced',
    species == 'New Kafe' ~ 'New Kafe'#,
  ))

# Now save these two final datasets into a single R object (a list)
data_list <- list(dat=dat,
                  joined_19_21=joined_19_21)

# Save that list as a R-data file (.rds)
saveRDS(data_list,'data_list.rds')

