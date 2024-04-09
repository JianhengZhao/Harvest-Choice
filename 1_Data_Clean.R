#Set a working directory for this model
setwd("")

# Input data
Data_V0 <- read.csv("Data_V0.csv",check.names=FALSE)

## Distance conversion to kilometers 
distance_columns <- c("Distance.roadsegment.", "Distance.Rail.", "Distance.national_highway.", "Distance.State.road.")
for (col in distance_columns) {
  Data_V0[[col]] <- Data_V0[[col]] * 111.32
}


## Land value acre to ha ##
Data_V0$AVG_val_ac<-Data_V0$AVG_val_ac*2.47


## Other conversion 
Data_V0$County<-as.factor(Data_V0$County)
Data_V0$Year<-as.factor(Data_V0$Year)
Data_V0$ELEV<-as.numeric(Data_V0$ELEV)

write.csv(Data_V0, "Data_Clean_V0.csv", row.names = FALSE)
