######## Growth ########
rm(list=ls(all=TRUE))
library(readr)

# setting dictionary file
Growth <- read.csv("Data/Growth.csv",check.names=FALSE)


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
  coef(linearMod3)
}


# Process and analyze data for different types and years
process_and_analyze <- function(type) {
  data2006 <- process_data(Growth, "2006", type)
  data2011 <- process_data(Growth, "2011", type)
  data2016 <- process_data(Growth, "2016", type)
  
  combined_data <- rbind(data2006, data2011, data2016)
  perform_regression_analysis(combined_data)
  
}


# Outputs
biomass_types <- c("Tot", "Saw", "Pulp", "PulpLD", "Pallet")
results_df <- data.frame(BiomassType=character(), Intercept=numeric(), T0=numeric(), IT0_sqrt=numeric(), stringsAsFactors=FALSE)

for (type in biomass_types) {
  coefficients <- process_and_analyze(type)

  results_df <- rbind(results_df, data.frame(BiomassType=type, Intercept=coefficients["(Intercept)"], T0=coefficients["T0"], IT0_sqrt=coefficients["I(T0^(1/2))"]))
}


##  Sawlog Growth 
Data_Clean_V0$post<-Data_Clean_V0$L_BioSaw-Data_Clean_V0$HarvSawlog
Data_Clean_V0$postend<-results_df[2,2]+results_df[2,3]*Data_Clean_V0$post+results_df[2,4]*Data_Clean_V0$post^(1/2)
Data_Clean_V0$postGrowth<-Data_Clean_V0$postend-Data_Clean_V0$post
Data_Clean_V0$postGrowth_sqr<-Data_Clean_V0$postGrowth*Data_Clean_V0$postGrowth
##  Pulpwood Growth 
Data_Clean_V0$postp<-Data_Clean_V0$L_BioPulp-Data_Clean_V0$HarvPulp
Data_Clean_V0$postendp<-results_df[3,2]+results_df[3,3]*Data_Clean_V0$postp+results_df[3,4]*Data_Clean_V0$postp^(1/2)
Data_Clean_V0$postGrowthp<-Data_Clean_V0$postendp-Data_Clean_V0$postp
Data_Clean_V0$postGrowth_sqrp<-Data_Clean_V0$postGrowthp*Data_Clean_V0$postGrowthp

write.csv(Data_Clean_V0, "Data_Clean_V1.csv")
