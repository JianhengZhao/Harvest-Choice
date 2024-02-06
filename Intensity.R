# Pakages
install.packages("VGAM")
library(stats4)
library(splines)
library(VGAM)


# setting dictionary file
Data_Clean <- read.csv("Data/Data_Clean.csv",check.names=FALSE)

#sawlog intensity
Data_Saw<-subset(Data_Clean, ratioSawlog>0&ratioSawlog<0.7)
I1 <- vglm(ratioSawlog ~ L_BioSaw+I(L_BioSaw^2)+postGrowth+I(postGrowth^2), tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Saw, control = vglm.control(epsilon = 1e-6))
## Sampled data
Data_Saw$Sawintensity<-predict(I1, newdata=Data_Saw, type="response")
plot(density(Data_Saw$ratioSawlog, na.rm=TRUE))
plot(density(Data_Saw$Sawintensity, na.rm=TRUE))
with(Data_Saw, cor(Sawintensity, ratioSawlog))
scatter.smooth(x=Data_Saw$Sawintensity, Data_Saw$ratioSawlog,col="red", main="Growth")

## All data
Data_Clean$Sawintensity<-predict(I1, newdata=Data_Clean, type="response")
Data_Clean2<-subset(Data_Clean, ratioSawlog>0&ratioSawlog<0.7)
with(Data_Clean2, cor(Sawintensity, ratioSawlog))



#Pulp intensity
Data_Pulp<-subset(Data_Clean, ratioPulp>0&ratioPulp<0.7)
summary(I2 <- vglm(ratioPulp ~  PriceSaw_county+PricePulp_county
                   +L_BioTot +BioPulpLD+postGrowthp+postGrowth_sqrp+Conservation_Type, tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Pulp))

## Sampled data
Data_Pulp$Pulpintensity<-predict(I2, newdata=Data_Pulp, type="response")
plot(density(Data_Pulp$ratioPulp, na.rm=TRUE))
plot(density(Data_Pulp$Pulpintensity, na.rm=TRUE))
with(Data_Pulp, cor(Pulpintensity, ratioPulp))


## All data
Data_Clean$Pulpintensity<-predict(I2, newdata=Data_Clean, type="response")
Data_Clean2<-subset(Data_Clean, ratioPulp>0&ratioPulp<0.7)
with(Data_Clean2, cor(Pulpintensity, ratioPulp))
scatter.smooth(x=Data_Clean$Pulpintensity, Data_Clean$ratioPulp,col="red", main="Growth")



















