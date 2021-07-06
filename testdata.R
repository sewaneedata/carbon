library (dplyr)
library(googlesheets4)
library (tidyr)
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

dat <- dat %>% select (- "...16")
dat <- dat %>% select (-"...25")
dat <- dat %>% drop_na()
# Data now ready for app, no NAs


