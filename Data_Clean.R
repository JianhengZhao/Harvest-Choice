# Input data
Data_Clean <- read.csv("Data/DataV0.csv",check.names=FALSE)

## Convert dependent variables to factors 
columns <- c("ChoiceSawlog", "ChoicePulp", "ChoiceLD", "ChoiceTot")
for (col in columns) {
  Data_Clean[[col]] <- as.factor(Data_Clean[[col]])
  levels(Data_Clean[[col]]) <- c("None", "Partial", "Final")
}

## Distance conversion to kilometers 
distance_columns <- c("Distance.roadsegment.", "Distance.Rail.", "Distance.national_highway.", "Distance.State.road.")
for (col in distance_columns) {
  Data_Clean[[col]] <- Data_Clean[[col]] * 111.32
}
## Feet to Meter ##
Data_Clean$ELEV<-Data_Clean$ELEV*0.3048

## Land value acre to ha ##
Data_Clean$AVG_val_ac<-Data_Clean$AVG_val_ac*2.47

## Conservation Land
Data_Clean$Conservation_Type<-as.factor(Data_Clean$Conservation_Type )
levels(Data_Clean$Conservation_Type ) <- c("non-conserved","Private","Public")

## Other conversion 
Data_Clean$County<-as.factor(Data_Clean$county)
Data_Clean$Year<-as.factor(Data_Clean$year)
Data_Clean$ELEV<-as.numeric(Data_Clean$ELEV)

# Output
write.csv(Data_Clean, "Data_Clean.csv")

