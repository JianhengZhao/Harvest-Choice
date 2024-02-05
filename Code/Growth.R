######## Growth ########
rm(list=ls(all=TRUE))
library(readr)

# Import file
Growth <- read.csv("C:/Users/Jianheng/OneDrive - University of Maine System/Desktop/Paper_Project/3.Timber_Supply/1.Harvest_Model/2.Data/Growth_LD_2nd_NA.csv",check.names=FALSE)


# setting dictionary file

setwd("C:/Users/Jianheng/OneDrive - University of Maine System/Desktop/Paper_Project/3.Timber_Supply/Organized Codes")
getwd()
Growth <- read.csv("Data/Growth_V0.csv",check.names=FALSE)


# Function to process data (Total  & Biomass & Pulp & Pallet & PulpLD)
process_data <- function(Growth, year, type) {
  previous_year <- as.character(as.numeric(year) - 5)
  
  if (type == "PulpLD") {
    selected_data <- subset(Growth, Growth[[paste0(year, "_PulpH")]] == 0 & Growth[[paste0(year, "_LDH")]] == 0)
    selected_data[[paste0(previous_year, "_PulpLDB")]] <- selected_data[[paste0(previous_year, "_LDB")]] + selected_data[[paste0(previous_year, "_PulpB")]]
    selected_data[[paste0(year, "_PulpLDB")]] <- selected_data[[paste0(year, "_LDB")]] + selected_data[[paste0(year, "_PulpB")]]
    selected_data$T0 <- selected_data[[paste0(previous_year, "_PulpLDB")]]
    selected_data$T1 <- selected_data[[paste0(year, "_PulpLDB")]]
  } else {
    type_H <- paste0(year, "_", type, "H")
    type_B <- paste0(year, "_", type, "B")
    prev_type_B <- paste0(previous_year, "_", type, "B")
    selected_data <- subset(Growth, Growth[[type_H]] == 0 & Growth[[type_B]] != 0 & Growth[[prev_type_B]] != 0)
    selected_data$T0 <- selected_data[[prev_type_B]]
    selected_data$T1 <- selected_data[[type_B]]
  }
  
  return(selected_data[, c("T0", "T1")])
}


# Function to perform growth regression analysis
perform_regression_analysis <- function(data) {
  linearMod1 <- lm(T1 ~ T0, data = data)
  linearMod2 <- lm(T1 ~ T0 + I(T0^2), data = data)
  linearMod3 <- lm(T1 ~ T0 + I(T0^(1/2)), data = data)
  
  print(summary(linearMod1))
  print(summary(linearMod2))
  print(summary(linearMod3))
  print(summary(data$T0))
  print(summary(data$T1))
}


# Process and analyze data for different types and years
process_and_analyze <- function(type) {
  data2006 <- process_data(Growth, "2006", type)
  data2011 <- process_data(Growth, "2011", type)
  data2016 <- process_data(Growth, "2016", type)
  
  combined_data <- rbind(data2006, data2011, data2016)
  perform_regression_analysis(combined_data)
  
}

# Print out for different types

process_and_analyze("Tot")     # For Total Biomass
process_and_analyze("Saw")     # For Sawlog
process_and_analyze("Pulp")    # For Pulp
process_and_analyze("PulpLD")  # For PulpLD
process_and_analyze("Pallet")  # For Pallet


