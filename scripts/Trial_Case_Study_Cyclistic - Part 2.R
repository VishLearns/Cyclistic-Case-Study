## Part 1.2 - Preparation of Cyclistic Data for Data Analysis

   # Creating a copy of the original file.
   

install.packages("readr") # safe practice to install, even though under R 4.5.1 version 'readr' is built in.
install.packages("tidyverse")

library(readr) # no need to load, as it shows warning message of already build.
library(tidyverse) # to work with tidyverse tools.

# Step 1: Get the original combined data
combined_data <- read.csv("C:/Users/vishv/Downloads/Cyclistic Case Study/Data for Cyclistic/Trial 2 - In Progress/Cyclistic Data - Combined Using R/Combined_Data.csv")

glimpse(combined_data) # get summary of the loaded data

working_data <- combined_data # transfer data from variable to variable

glimpse(working_data) # confirm summary of the loaded data in the new variable

write_csv(working_data,"C:/Users/vishv/Downloads/Cyclistic Case Study/Data for Cyclistic/Trial 2 - In Progress/Cyclistic Data - Combined Using R/Working_Data.csv")
# create a csv file path and name to write a .csv file.

summary(working_data) # summary of the variable
glimpse(working_data) # glimpse verification
view(working_data)    # preview of the table