library (gsheet)
library (dplyr)
df <- gsheet2tbl ('https://drive.google.com/file/d/1_Um0R5l1VzfhTWhy1XiK6H-jlHysmWoN/view?usp=sharing')         

?gsheet2tbl
df <- read_sheet ('https://drive.google.com/file/d/1_Um0R5l1VzfhTWhy1XiK6H-jlHysmWoN/view?usp=sharing', sheet = 2)
View (df)
library (googlesheets4)
sheet_numbers <- 5:53
data_list <- list()
for(i in 1:length(sheet_numbers)){
  sheet_id <- sheet_numbers[i]
  data_list[[i]]<- read_sheet('https://docs.google.com/spreadsheets/d/1QMTqhd-llPQH_2AfaiV6klHXpe1alPE7Eo85UU82sP0/edit?usp=sharing',
                              sheet = sheet_id)
  print(i)
}

library(googlesheets4)
data_list <- list()
sheet_numbers <- 5:53
for(i in 1:length(sheet_numbers)){
  sheet_id <- sheet_numbers[i]
  temp <- read_sheet('https://docs.google.com/spreadsheets/d/1QMTqhd-llPQH_2AfaiV6klHXpe1alPE7Eo85UU82sP0/edit?usp=sharing', sheet = sheet_id)
  temp <- temp[,1:30]
  data_list[[i]] <- temp
}
dat <- do.call('rbind', data_list)

# Info on all of the data- household names and numbers 
meta <- gs4_get("https://docs.google.com/spreadsheets/d/1QMTqhd-llPQH_2AfaiV6klHXpe1alPE7Eo85UU82sP0/edit?usp=sharing")

meta

library (tidyr)

dat <- dat %>% select (- "...16")
dat <- dat %>% select (-"...25")
dat <- dat %>% drop_na()
# Data now ready for app, no NAs


