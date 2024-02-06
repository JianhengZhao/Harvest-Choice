##### Harvest Choice

setwd("C:/Users/Jianheng/OneDrive - University of Maine System/Desktop/Paper_Project/3.Timber_Supply/Organized Codes")
getwd()

Data_Clean_V2 <- read.csv("Data_Clean_V2.csv",check.names=FALSE)
library("nnet") 

# Relevel the factor to make 'None' the reference level
columns <- c("ChoiceSawlog", "ChoicePulp", "ChoiceLD", "ChoiceTot")
for (col in columns) {
  new_col_name <- paste0(col, "0") # Create a new column name by appending '0'
  Data_Clean_V2[[new_col_name]] <- relevel(Data_Clean_V2[[col]], ref= "None")
}

## (m,p) Vs (m1,p1) Distance high way ##
#************************************#
m5 <- multinom(ChoiceSawlog0  ~ PriceSaw_county + PricePulp_county  + L_BioTot + BioTot +postGrowth+postGrowth_sqr+SWMills +AVG_val_ac +Distance.national_highway.+County +Conservation_Type+ELEV+Year+Coastal, data =Data_Clean_V2,na.action = na.exclude )
stargazer(m5, title="Sawlog Combined", align=TRUE, dep.var.labels=c("Thinning","Final"),
          no.space=TRUE, type="text", out="m.htm")

#************************************#

p5 <-multinom(ChoicePulp0  ~ PriceSaw_county + PricePulp_county +L_BioTot +BioPulpLD+postGrowthp+postGrowth_sqrp+PulpMills+AVG_val_ac+Distance.national_highway.+County+Conservation_Type +ELEV+Year+Coastal, data =Data_Clean_V2,na.action = na.exclude )

stargazer(p5, title="pulpwood Combined", align=TRUE, dep.var.labels=c("Thinning","Final"),
          no.space=TRUE, type="text", out="p1.htm")


#### Validation
## Deviance, log-likelihood and AIC
deviance(p)
logLik(p)
AIC(p)
## Log-likelihoods for full model and 0-model without predictors ##
m0<-multinom(ChoiceSawlog0  ~ 1, data =mydata,na.action = na.exclude)
LLf<-logLik(m)
LL0<-logLik(m0)
p0<-multinom(ChoicePulp0  ~ 1, data =mydata,na.action = na.exclude)
LLf<-logLik(p)
LL0<-logLik(p0)
## McFadden pseudo-R2
as.vector(1 - (LLf / LL0))