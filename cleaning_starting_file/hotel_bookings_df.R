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

# Showing unique values from is_repeated_guest column
unique_is_repeated_guest <- unique(df$is_repeated_guest)
print(unique_is_repeated_guest)

# Changing values from binary to yes/no in is_repeated_guest column
df <- df %>% 
  mutate(is_repeated_guest = case_when(
    is_repeated_guest == 1 ~ "yes",
    is_repeated_guest == 0 ~ "no",
    TRUE ~ as.character(is_repeated_guest)  # handle other cases if any
  ))

# Showing unique values from previous_cancellations column
unique_previous_cancellations <- unique(df$previous_cancellations)
print(unique_previous_cancellations)
# No need to change values in column

# Showing unique values from previous_bookings_not_canceled column
unique_previous_bookings_not_canceled <- unique(df$previous_bookings_not_canceled)
print(unique_previous_bookings_not_canceled)
# No need to change values in column

# Showing unique values from agent column
unique_agent <- unique(df$agent)
print(unique_agent)

# Showing unique values from company column
unique_company <- unique(df$company)
print(unique_company)

# Dropping agent and company columns
df <- df %>%
  select(-agent, -company)

# Identify missing values in the dataset
df %>% 
  summarise_all(~sum(is.na(.)))

# Dropping all rows with NA values
df <- na.omit(df)

# Renaming hotel column to hotel_type
df <- df %>%
  rename(hotel_type = hotel)

# Renaming lead_time column to from_booking_to_arrival
df <- df %>%
  rename(from_booking_to_arrival = lead_time)

# Showing unique values from arrival_date_day_of_month column
unique_arrival_date_day_of_month <- unique(df$arrival_date_day_of_month)
print(unique_arrival_date_day_of_month)

# Convert month names to numerical values
month_num <- match(tolower(df$arrival_date_month), tolower(month.name))
df$arrival_date_month <- ifelse(is.na(month_num), NA, month_num)

# Combine 'arrival_date_year', 'arrival_date_month', and 'arrival_date_day_of_month' into 'arrival_date'
df$arrival_date <- as.Date(paste(df$arrival_date_year, df$arrival_date_month, df$arrival_date_day_of_month, sep="-"))

# Move 'arrival_date' column to the 4th position
df <- df %>% select(1:3, arrival_date, 4:length(df))

# Dropping agent and company columns
df <- df %>%
  select(-arrival_date_year, -arrival_date_month, -arrival_date_day_of_month)