# Laura Goyeneche
# August 30, 2018
# H1 R Shiny - data cleaning
# -------------------------------------------------------------------

rm(list = ls())

# Libraries
library(dplyr)
library(tidyverse)
library(readxl)

# Working directory
cd = "/Users/lgoye/OneDrive/Documents/GitHub/hw1_lgoyenec"

# Import each data
# Add string variable with 'violent crime' names
# Create master data with all 
varsName = c("kidnapping","robbery","serious_assault","sexual_exploitation")
data     = c()
for (i in varsName) {
  dataTemp        = read_excel(paste0(cd,"/data_xls/",i,".xlsx"))
  dataTemp        = dataTemp %>% mutate(crimename = i)
  names(dataTemp) = tolower(names(dataTemp))
  data            = rbind(data,dataTemp)
}

# Modify labels in `crimename` column
# Variable `year` as numeric
# Calculate `rate`: different normalization per variable
data = 
  data %>%
  mutate(crimename = gsub("_"," ",crimename),
         crimename = str_to_sentence(crimename),
         year = as.numeric(year),
         rate = rate/100000)

# Modify names of variable
names(data) = str_to_sentence(gsub("_","",names(data)))

# Save dataset in R format
saveRDS(data, paste0(cd,"/app/master_data.rds"))
# -------------------------------------------------------------------