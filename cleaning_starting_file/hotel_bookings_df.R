# Importing necessary libraries
library(tidyverse)

# Specify the relative path to your dataset within the project
dataset_path <- "starting_file/hotel_bookings.csv"

# Import the dataset using read.csv with a.strings parameter to indicate the values that should be treated as missing (blank)
df <- read.csv(dataset_path, na.strings = c("", "NA"))

# View the contents of a dataframe
View(df)

# Inspect the structure of the data
glimpse(df)

# Showing unique values from is_canceled column
unique_is_canceled <- unique(df$is_canceled)
print(unique_is_canceled)

# Changing values from binary to yes/no in is_cancelled column
df <- df %>% 
  mutate(is_canceled = case_when(
    is_canceled == 1 ~ "yes",
    is_canceled == 0 ~ "no",
    TRUE ~ as.character(is_canceled)  # handle other cases if any
  ))