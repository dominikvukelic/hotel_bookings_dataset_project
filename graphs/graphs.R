# Importing necessary libraries
library(ggplot2)
library(tidyverse)  # Includes dplyr and forcats
library(scales)
library(ggrepel)


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
  geom_bar(position = "dodge", color = "black") +
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

# Create a histogram for stay durations
ggplot(data = df, aes(x = total_stay)) +
  geom_histogram(binwidth = 1, fill = "limegreen", color = "black") +
  labs(title = "Distribution of Stay Duration", x = "Stay Duration (days)", y = "Frequency")

# Reshape the data for plotting
df_long <- pivot_longer(df, cols = c(stays_in_weekend_nights, stays_in_week_nights), names_to = "Stay_Type", values_to = "Stay_Duration")

# Create a histogram for stay durations with separate facets for weekend and weekday
ggplot(data = df_long, aes(x = Stay_Duration, fill = Stay_Type)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  labs(title = "Distribution of Stay Duration", x = "Stay Duration (days)", y = "Frequency") +
  facet_wrap(~ Stay_Type, scales = "free_y", labeller = labeller(Stay_Type = c("stays_in_weekend_nights" = "Weekend Stay", "stays_in_week_nights" = "Week Stay"))) +
  scale_fill_manual(values = c("stays_in_weekend_nights" = "blue", "stays_in_week_nights" = "red"), name = "Stay Type")

# Create a bar plot for meal type
ggplot(data = df, aes(x = meal, fill = factor(meal))) +
  geom_bar(position = position_dodge(width = 0.9), stat = "count", color = "black") +  # Bar plot
  geom_text(stat = "count", aes(label = stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +  # Display numbers above bars
  labs(title = "Meal Type Preference", x = "", y = "") +  # Remove axis labels
  scale_fill_discrete(name = "Meal Type") +  # Show value names in legend
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),  # Remove axis values
        axis.title.x = element_blank(), axis.title.y = element_blank(),  # Remove axis titles
        axis.ticks = element_blank())  # Remove axis ticks

# Create a violin plot to compare stay duration for previous and non-previous guests
ggplot(data = df, aes(x = previous_guest, y = total_stay, fill = previous_guest)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Stay Duration vs. Previous Guest Status", x = "Previous Guest", y = "Stay Duration (days)")

# Compute counts by month and hotel type
df_counts <- df %>%
  group_by(month = factor(month(arrival_date, label = TRUE)), hotel_type) %>%
  summarise(count = n())

# Create a bar plot for booking distribution by month
ggplot(data = df_counts, aes(x = month, y = count, fill = hotel_type)) +
  geom_col(position = position_dodge(width = 1), color = "black", width = 0.9) +  # Adjust width
  geom_text(aes(label = count, group = hotel_type),
            position = position_dodge(width = 1), vjust = -0.5, size = 3) +  # Add numbers above bins
  labs(title = "Booking Distribution by Month", x = "Month", y = "Count", fill = "Hotel Type") +
  scale_x_discrete(labels = month.abb)  # Display month names in English


# Filter the original data for the top 10 countries
top_countries <- df %>%
  group_by(country) %>%
  summarise(total_count = n()) %>%
  arrange(desc(total_count)) %>%
  head(10)

filtered_data <- df %>%
  filter(country %in% top_countries$country)

# Create a grouped bar plot for the top 10 countries showing the counts of each hotel_type
ggplot(data = filtered_data, aes(x = country, fill = hotel_type)) +
  geom_bar(position = position_dodge(width = 0.8), color = "black", width = 0.7) +  # Adjust width and add black outline
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +  # Add numbers above bars
  labs(title = "Hotel Type Distribution in Top 10 Countries", x = "Country", y = "Count", fill = "Hotel Type") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  scale_fill_brewer(palette = "Set2")  # Use a color palette from RColorBrewer

# Create a new column to categorize guests based on their characteristics
df <- df %>%
  mutate(guest_type = case_when(
    adults > 0 & children == 0 & babies == 0 ~ "Adults Only",
    adults > 0 & children > 0 & babies == 0 ~ "Adults with Children",
    adults > 0 & babies > 0 & children == 0 ~ "Adults with Babies",
    adults > 0 & children > 0 & babies > 0 ~ "Adults with Children and Babies",
    TRUE ~ "Other"
  ))

# Filter data for non-canceled reservations
non_canceled_data <- df %>%
  filter(is_canceled == "no")

# Create a grouped bar chart for non-canceled reservations
ggplot(data = non_canceled_data, aes(x = guest_type, fill = guest_type)) +
  geom_bar(color = "black", position = position_dodge(width = 0.9), width = 0.7) +  # Add black outline
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Add count labels
  labs(title = "Guest Type Distribution for Non-Canceled Reservations") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +  # Remove x and y-axis labels
  scale_fill_brewer(palette = "Set2")  # Use a color palette from RColorBrewer

# Create a histogram for booking_changes
ggplot(data = df, aes(x = booking_changes)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +  # Show count above each bin
  scale_x_continuous(breaks = unique(df$booking_changes)) +  # Set x-axis ticks to unique values
  labs(title = "Distribution of Booking Changes", x = "Booking Changes", y = "Frequency")