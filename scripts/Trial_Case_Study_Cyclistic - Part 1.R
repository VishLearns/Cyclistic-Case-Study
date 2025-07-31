## Part 1 - Preparation of Cyclistic Data for Data Analysis - 05/07/2025

install.packages("tidyverse")
library(tidyverse) # Used for data manipulation
install.packages("readr")
library(readr) # Used for rds file creation

# STEP 1: Set your folder path

folder_path <- choose.dir() 
  # using choose.dir() function so that I can directly select the folder with all files - minimizing syntax error.

# STEP 2: Get the full list of CSV files

all_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  # Look in this folder, find all the .csv files, and give me their full file paths as a list.
  # Use "full.names = True" for full path, which makes the code more efficient. - a good backup safe coding practice. 

length(all_files)
  # to check if total number of .csv files are same as the length

# STEP 3: Combine them into one big data frame
combined_data <- all_files %>%
  map_dfr(read_csv, show_col_types = FALSE)
  # creates a "combined_data" variable that stores a big table, which was formed by mapping files from "all files,"
  # reading all the csv files, stacking them vertically through (Data Frame by Rows)dfr instead of dfc, 
  # and hiding the Column Type pop-ups through "show_col_types = FALSE" condition.

# STEP 4: Preview the structure
glimpse(combined_data)
  # glimpse() gives better summary of the data than str() because it shows horizontal view.

# STEP 5: Save the file.
write_csv(combined_data, file.path(choose.dir(), "Combined_Data.csv"))
  # allows you to create a CSV file of the big table in the combined_data variable.
  # filename and file-extension should be mentioned OR give full path of the folder, including file name.
  # drawback observed was that .csv file was of 1.24Gb - which is huge file size and may impact processing time.

write_rds(combined_data, "C:/Users/vishv/Downloads/Cyclistic Case Study/Data for Cyclistic/Trial 2 - In Progress/Combined_Data.rds")
 # Used it but the file size is even bigger than the csv file.
 # Upon further reading, I come to understand that csv file is more sharable than rds, so I will continue analysis on it.

## Thank you for reading the first part of my preparation process.

