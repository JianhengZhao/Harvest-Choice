# Pakages
install.packages("VGAM")
library(stats4)
library(splines)
library(VGAM)

# setting dictionary file
Data_Clean_V1 <- read.csv("Data/Data_Clean_V1.csv",check.names=FALSE)

#sawlog intensity
Data_Saw <- subset(Data_Clean_V1, ratioSawlog > 0 & ratioSawlog < 0.7)
I1 <- vglm(ratioSawlog ~ L_BioSaw+I(L_BioSaw^2)+postGrowth+I(postGrowth^2), tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Saw, control = vglm.control(epsilon = 1e-6))
## Sampled data
Data_Saw$Sawintensity<-predict(I1, newdata=Data_Saw, type="response")
plot(density(Data_Saw$ratioSawlog, na.rm=TRUE))
plot(density(Data_Saw$Sawintensity, na.rm=TRUE))
with(Data_Saw, cor(Sawintensity, ratioSawlog))
scatter.smooth(x=Data_Saw$Sawintensity, Data_Saw$ratioSawlog,col="red", main="Growth")

## All data
Data_Clean_V1$Sawintensity<-predict(I1, newdata=Data_Clean_V1, type="response")
Data_Clean_temp<-subset(Data_Clean_V1, ratioSawlog>0&ratioSawlog<0.7)
with(Data_Clean_temp, cor(Sawintensity, ratioSawlog))


#Pulp intensity
Data_Pulp<-subset(Data_Clean_V1, ratioPulp>0&ratioPulp<0.7)
I2 <- vglm(ratioPulp ~  PriceSaw_county+PricePulp_county+
                   L_BioTot +BioPulpLD+postGrowthp+postGrowth_sqrp+Conservation_Type, tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Pulp)

## Sampled data
Data_Pulp$Pulpintensity<-predict(I2, newdata=Data_Pulp, type="response")
plot(density(Data_Pulp$ratioPulp, na.rm=TRUE))
plot(density(Data_Pulp$Pulpintensity, na.rm=TRUE))
with(Data_Pulp, cor(Pulpintensity, ratioPulp))

## All data
Data_Clean_V1$Pulpintensity<-predict(I2, newdata=Data_Clean_V1, type="response")
Data_Clean_temp<-subset(Data_Clean_V1, ratioPulp>0&ratioPulp<0.7)
with(Data_Clean_temp, cor(Pulpintensity, ratioPulp))
scatter.smooth(x=Data_Clean_V1$Pulpintensity, Data_Clean_V1$ratioPulp,col="red", main="Growth")

write.csv(Data_Clean_V1, "Data_Clean_V2.csv", row.names = FALSE)














