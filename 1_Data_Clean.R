#Set a working directory for this model
setwd("")

# Input data
Data_V0 <- read.csv("Data_V0.csv",check.names=FALSE)

## Convert dependent variables to factors 
columns <- c("ChoiceSawlog", "ChoicePulp", "ChoiceLD", "ChoiceTot")
for (col in columns) {
  Data_V0[[col]] <- as.factor(Data_V0[[col]])
  levels(Data_V0[[col]]) <- c("None", "Partial", "Final")
}

## Distance conversion to kilometers 
distance_columns <- c("Distance.roadsegment.", "Distance.Rail.", "Distance.national_highway.", "Distance.State.road.")
for (col in distance_columns) {
  Data_V0[[col]] <- Data_V0[[col]] * 111.32
}
## Feet to Meter ##
Data_V0$ELEV<-Data_V0$ELEV*0.3048

## Land value acre to ha ##
Data_V0$AVG_val_ac<-Data_V0$AVG_val_ac*2.47

## Conservation Land
Data_V0$Conservation_Type<-as.factor(Data_V0$Conservation_Type )
levels(Data_V0$Conservation_Type ) <- c("non-conserved", "Private", "Public")

## Other conversion 
Data_V0$County<-as.factor(Data_V0$County)
Data_V0$Year<-as.factor(Data_V0$Year)
Data_V0$ELEV<-as.numeric(Data_V0$ELEV)

write.csv(Data_V0, "Data_Clean_V0.csv", row.names = FALSE)
