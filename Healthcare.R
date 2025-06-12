# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# Load the dataset
healthcare_data <- read.csv("'/Users/naincysingh/Library/Mobile Documents/com~apple~Numbers/Documents/healthcare_dataset.csv'")

# Data Preprocessing
healthcare_data$year_established <- as.integer(healthcare_data$year_established)  # Convert year_established to integer

# Check for missing values
missing_values <- colSums(is.na(healthcare_data))
print("Missing Values:")
print(missing_values)

# Summary of the dataset
print("Dataset Summary:")
print(summary(healthcare_data))

# Oldest and Newest Healthcare Facilities
oldest_facility <- healthcare_data[which.min(healthcare_data$year_established), ]
newest_facility <- healthcare_data[which.max(healthcare_data$year_established), ]
print("Oldest Healthcare Facility:")
print(oldest_facility)
print("Newest Healthcare Facility:")
print(newest_facility)

# Calculate Facility Age
current_year <- as.integer(format(Sys.Date(), "%Y"))
healthcare_data$facility_age <- current_year - healthcare_data$year_established

# Calculate Average Age of Facilities
avg_age <- mean(healthcare_data$facility_age, na.rm = TRUE)
print("Average Facility Age:")
print(avg_age)

# Count Facilities by Type
facility_type_count <- healthcare_data %>% group_by(facility_type) %>% summarise(count = n()) %>% arrange(desc(count))
print("Facility Count by Type:")
print(facility_type_count)

# Visualization 1: Healthcare Facility Establishment Trend
g1 <- ggplot(healthcare_data, aes(x = year_established)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Healthcare Facility Establishment Trend", x = "Year Established", y = "Count of Facilities") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Visualization 2: Distribution of Healthcare Facilities by State
g2 <- ggplot(healthcare_data, aes(x = reorder(state, table(state)[state]), fill = state)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Number of Healthcare Facilities per State", x = "State", y = "Count") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Visualization 3: Facility Age Analysis
g3 <- ggplot(healthcare_data, aes(x = reorder(name, facility_age), y = facility_age, fill = facility_age)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Healthcare Facilities by Age", x = "Facility", y = "Years Since Establishment") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Visualization 4: Facility Count by Type
g4 <- ggplot(facility_type_count, aes(x = reorder(facility_type, count), y = count, fill = facility_type)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Healthcare Facilities by Type", x = "Facility Type", y = "Count") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Visualization 5: Facility Age Distribution
g5 <- ggplot(healthcare_data, aes(x = facility_age)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Facility Age Distribution", x = "Years Since Establishment", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Arrange all plots in a grid
grid.arrange(g1, g2, g3, g4, g5, ncol = 2)
