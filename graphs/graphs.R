# Importing necessary libraries
library(ggplot2)
library(tidyverse)  # Includes dplyr and forcats
library(scales)


# Specifying the relative path to your dataset within the project
dataset_path <- "cleaned_starting_file/hotel_bookings_cleaned.csv"

# Importing the dataset using read.csv with na.strings parameter to indicate missing values
df <- read.csv(dataset_path, na.strings = c("", "NA"))

# Viewing the contents of a dataframe
View(df)

# Calculate percentages of cancelations using dplyr
df_percent <- df %>%
  count(is_canceled) %>%
  mutate(percentage = n / sum(n) * 100)

# Creating a pie chart with percentages for cancelations
ggplot(df_percent, aes(x = "", y = percentage, fill = is_canceled)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Cancellation Status",
       fill = "Cancellation Status") +
  scale_fill_manual(values = c("yes" = "lightpink", "no" = "lightblue")) +  # Switched colors
  theme_void()

# Grouped Bar Plot for Cancellation Counts by Hotel Type
ggplot(data = df, aes(x = hotel_type, fill = factor(is_canceled))) +
  geom_bar(position = "dodge") +
  labs(title = "Cancellation Counts by Hotel Type", y = "Count", fill = "Canceled or not") +
  scale_fill_manual(values = c("yes" = "maroon", "no" = "skyblue"))

# Convert 'arrival_date' to Date format if it's not already
df$arrival_date <- as.Date(df$arrival_date)

# Set the locale to English
Sys.setlocale("LC_TIME", "English")

# Create a plot with months on the x-axis and a colorful theme
ggplot(data = df, aes(x = arrival_date)) +
  geom_line(stat = "count", color = "blue") +  # You can specify a color for the line
  labs(title = "Booking Occurrence Over Time", x = "Booking Date", y = "Number of Bookings") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal()  +  # Change the theme to a colorful minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels

