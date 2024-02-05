# Pakages
install.packages("VGAM")
library(stats4)
library(splines)
library(VGAM)


# setting dictionary file

setwd("C:/Users/Jianheng/OneDrive - University of Maine System/Desktop/Paper_Project/3.Timber_Supply/Organized Codes")
getwd()
mydata <- read.csv("Data/mydata.csv",check.names=FALSE)

#sawlog intensity
Data_Saw<-subset(mydata, ratioSawlog>0&ratioSawlog<0.7)



Data_Saw_try <- data.frame(ratioSawlog=Data_Saw$ratioSawlog, L_BioSaw= Data_Saw$L_BioSaw,
                           PriceSaw_county = Data_Saw$PriceSaw_county, PricePulp_county =Data_Saw$PricePulp_county,
                           Distance.national_highway. = Data_Saw$Distance.national_highway.,
                           ELEV = Data_Saw$ELEV,postGrowth= Data_Saw$postGrowth, postGrowth_sqr= Data_Saw$postGrowth_sqr,
                           SWMills = Data_Saw$SWMills, AVG_val_ac = Data_Saw$AVG_val_ac,
                           L_BioTot = Data_Saw$L_BioTot, BioTot = Data_Saw$BioTot,
                           Conservation_Type = Data_Saw$Conservation_Type,
                           Sawintensity = Data_Saw$Sawintensity,
                           stands = Data_Saw$stands)

I1 <- vglm(ratioSawlog ~ L_BioSaw + PriceSaw_county + PricePulp_county +
             Distance.national_highway. + ELEV + postGrowth +postGrowth_sqr+SWMills +AVG_val_ac, 
           tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Saw_try, control = vglm.control(epsilon = 1e-6))

summary(I1 <- vglm(ratioSawlog ~ L_BioSaw+I(L_BioSaw^2)+postGrowth+I(postGrowth^2), tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Saw_try, control = vglm.control(epsilon = 1e-6)))

summary(I1 <- vglm(ratioSawlog ~ stands +I(stands^2)+PriceSaw_county + PricePulp_county
                   +Conservation_Type, tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Saw_try))




summary(I1)

## Sampled data
Data_Saw_try$Sawintensity2<-predict(I1, newdata=Data_Saw_try, type="response")
plot(density(Data_Saw_try$ratioSawlog, na.rm=TRUE))
plot(density(Data_Saw_try$Sawintensity, na.rm=TRUE))
plot(density(Data_Saw_try$Sawintensity2, na.rm=TRUE))

r1 <- with(Data_Saw_try, cor(Sawintensity2, Sawintensity))
r2 <- with(Data_Saw_try, cor(Sawintensity2, ratioSawlog))
scatter.smooth(x=Data_Saw_try$Sawintensity, Data_Saw_try$Sawintensity2,col="red", main="Growth")
scatter.smooth(x=Data_Saw_try$Sawintensity, Data_Saw_try$Sawintensity2,col="red", main="Growth")
r1
r2


summary(Data_Saw_try$ratioSawlog)
summary(Data_Saw_try$Sawintensity)
summary(Data_Saw_try$Sawintensity2)

### All data
mydata$Sawintensity2<-predict(I1, newdata=mydata, type="response")
mydata2<-subset(mydata, ratioSawlog>0&ratioSawlog<0.7)
r1 <- with(mydata2, cor(Sawintensity2, Sawintensity))
scatter.smooth(x=mydata$Sawintensity, mydata$Sawintensity2,col="red", main="Growth")
r1
r2 <- with(mydata2, cor(Sawintensity2, ratioSawlog))
scatter.smooth(x=mydata$Sawintensity, mydata$Sawintensity2,col="red", main="Growth")
r2






##### #Pulp intensity
Data_Pulp<-subset(mydata, ratioPulp>0&ratioPulp<0.7)

Data_Pulp_try <- data.frame(ratioPulp=Data_Pulp$ratioPulp,
                           PriceSaw_county = Data_Pulp$PriceSaw_county, PricePulp_county =Data_Pulp$PricePulp_county,
                           Distance.national_highway. = Data_Pulp$Distance.national_highway.,
                           ELEV = Data_Pulp$ELEV,postGrowth= Data_Pulp$postGrowth, postGrowth_sqr= Data_Pulp$postGrowth_sqr,
                           SWMills = Data_Pulp$SWMills, AVG_val_ac = Data_Pulp$AVG_val_ac,
                           L_BioTot = Data_Pulp$L_BioTot, BioPulpLD = Data_Pulp$BioPulpLD,
                           Conservation_Type = Data_Pulp$Conservation_Type,
                           Pulpintensity = Data_Pulp$Pulpintensity,
                           postGrowthp =Data_Pulp$postGrowthp ,
                           postGrowth_sqrp=Data_Pulp$postGrowth_sqrp,
                           L_BioPulpLD =Data_Pulp$L_BioPulpLD,
                           standTp =Data_Pulp$standTp,
                           postGrowthTp =Data_Pulp$postGrowthTp,
                           postGrowth_sqrTp =Data_Pulp$postGrowth_sqrTp
                           )

summary(I2 <- vglm(ratioPulp ~  PriceSaw_county+PricePulp_county
                   +L_BioTot +BioPulpLD+postGrowthp+postGrowth_sqrp+Conservation_Type, tobit(Lower = 0, Upper = 0.7, type.f = "cens"), data = Data_Pulp_try))

## Sampled data
Data_Pulp_try$Pulpintensity2<-predict(I2, newdata=Data_Pulp_try, type="response")
plot(density(Data_Pulp_try$ratioPulp, na.rm=TRUE))
plot(density(Data_Pulp_try$Pulpintensity, na.rm=TRUE))
plot(density(Data_Pulp_try$Pulpintensity2, na.rm=TRUE))

r1 <- with(Data_Pulp_try, cor(Pulpintensity2, Pulpintensity))
r2 <- with(Data_Pulp_try, cor(Pulpintensity2, ratioPulp))
scatter.smooth(x=Data_Pulp_try$Pulpintensity, Data_Pulp_try$Pulpintensity2,col="red", main="Growth")
scatter.smooth(x=Data_Pulp_try$Pulpintensity, Data_Pulp_try$Pulpintensity2,col="red", main="Growth")
r1
r2
r3 <- with(Data_Pulp_try, cor(Pulpintensity, ratioPulp))
r3

summary(Data_Pulp$ratioPulp)
summary(Data_Pulp$Pulpintensity)
summary(Data_Pulp_try$Pulpintensity2)

summary(Data_Pulp_try$ratioPulp)
summary(Data_Pulp_try$Pulpintensity)
summary(Data_Pulp_try$Pulpintensity2)

### All data
mydata$Pulpintensity2<-predict(I2, newdata=mydata, type="response")
mydata2<-subset(mydata, ratioPulp>0&ratioPulp<0.7)
r1 <- with(mydata2, cor(Pulpintensity2, Pulpintensity))
scatter.smooth(x=mydata$Pulpintensity, mydata$Pulpintensity2,col="red", main="Growth")
r1
r2 <- with(mydata2, cor(Pulpintensity2, ratioPulp))
scatter.smooth(x=mydata$Pulpintensity, mydata$Pulpintensity2,col="red", main="Growth")
r2


















