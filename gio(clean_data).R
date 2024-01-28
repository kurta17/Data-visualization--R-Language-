# Set the working directory
setwd("C:\\Users\\think\\Downloads\\R_S2_2024\\R_S2_2024\\data")

# Load required packages
library(dplyr)
library(stringr)

# Read the dataset
electric_cars_data <- read.csv("el_car1.csv")

# Define a function to clean numeric columns
clean_numeric <- function(column) {
  cleaned_values <- as.numeric(gsub("[^0-9.]", "", column))
  cleaned_values[is.na(cleaned_values)] <- NA  
  cleaned_values
}

clean_numeric1 <- function(column) {
  cleaned_values <- as.numeric(gsub("[^0-9.]", "", column))
  cleaned_values[is.na(cleaned_values)] <- NA  
  cleaned_values
}

# Clean and transform the dataset
electric_cars_data <- electric_cars_data %>%
  slice(-10, -11) %>%
  mutate(Acceleration_sec = as.numeric(gsub(" sec", "", Acceleration))) %>%
  mutate(TopSpeed_kmh = as.numeric(gsub(" km/h", "", TopSpeed))) %>%
  mutate(Range_km = as.numeric(gsub(" km", "", Range))) %>%
  mutate(Efficiency_whkm = as.numeric(gsub(" Wh/km", "", Efficiency))) %>%
  mutate(FastchargeSpeed_kmh = clean_numeric1(FastChargeSpeed)) %>%
  mutate(PriceinGermany_euro = clean_numeric(PriceinGermany)) %>%
  mutate(PriceinUK_pound = clean_numeric(PriceinUK))

# Extract the year from the Available column
electric_cars_data <- electric_cars_data %>%
  mutate(Available = as.numeric(str_sub(Subtitle, start = -4)),
         d1 = substr(Subtitle, 1, 5),
         Useable_battery_Kwh2 = as.numeric(gsub("[^0-9.-]", "", d1)))

# Drop the original "Subtitle" column if it exists
if ("Subtitle" %in% colnames(electric_cars_data)) {
  electric_cars_data <- electric_cars_data %>%
    select(-Subtitle, -d1,
           -Acceleration,
           -TopSpeed,
           -Range,
           -Efficiency,
           -FastChargeSpeed,
           -PriceinGermany,
           -PriceinUK)
}

# Extract only numeric values from the Useable_battery_Kwh column
electric_cars_data$Useable_battery_Kwh <- clean_numeric(electric_cars_data$Useable_battery_Kwh)

# Extract only the year from the Available column
electric_cars_data$Available <- as.numeric(gsub("[^0-9]", "", electric_cars_data$Available))

# View the modified dataset
View(electric_cars_data)

output_file <- "mydata_project.csv"

# Save the dataframe to a CSV file
write.csv(electric_cars_data, file = output_file, row.names = FALSE)
# Calculate the mean of PriceinUK_pound
#mean_price_uk <- mean(electric_cars_data$PriceinUK_pound, na.rm = TRUE)
#cat("Mean Price in UK (Pound):", mean_price_uk, "\n")

