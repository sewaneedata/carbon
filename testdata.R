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

