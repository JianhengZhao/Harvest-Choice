install.packages("stargazer")
library("stargazer")
library("nnet") 

# Input data
Data_Clean_V2 <- read.csv("Data_Clean_V2.csv",check.names=FALSE)

## Convert dependent variables to factors 
columns <- c("ChoiceSawlog", "ChoicePulp", "ChoiceLD", "ChoiceTot")
for (col in columns) {
  Data_Clean_V2[[col]] <- as.factor(Data_Clean_V2[[col]])
  levels(Data_Clean_V2[[col]]) <- c("None", "Partial", "Final")
}

## Relevel the factor to make 'None' the reference level
columns <- c("ChoiceSawlog", "ChoicePulp", "ChoiceLD", "ChoiceTot")
for (col in columns) {
  new_col_name <- paste0(col, "0") # Create a new column name by appending '0'
  Data_Clean_V2[[new_col_name]] <- relevel(Data_Clean_V2[[col]], ref= "None")
}

Data_Clean_V2$Conserve<-as.factor(Data_Clean_V2$Conserve)
levels(Data_Clean_V2$Conserve) <- c("non-conserved","Conserved")

## aggregate Ownership 
Data_Clean_V2 <- Data_Clean_V2 %>%
  mutate(Ownership = as.factor(case_when(
    Ownership %in% c("STATE", "FEDERAL", "LOCAL") ~ "Government",
    TRUE ~ as.character(Ownership)
  )))
Data_Clean_V2$Ownership<-as.factor(Data_Clean_V2$Ownership)
Data_Clean_V2$Ownership <- relevel(Data_Clean_V2$Ownership, ref = "FAMILY")

#************************************#
S <- multinom(ChoiceSawlog0  ~ PriceSaw_county + PricePulp_county  + L_BioTot + BioTot +postGrowth+postGrowth_sqr+SWMills +AVG_val_ac +Distance.national_highway.+County +Conservation_Type+ELEV+Year+Coastal, data =Data_Clean_V2,na.action = na.exclude )
stargazer(S, title="Sawlog Combined", align=TRUE, dep.var.labels=c("Thinning","Final"),
          no.space=TRUE, type="text", out="Sawlog.htm")

#************************************#

P <-multinom(ChoicePulp0  ~ PriceSaw_county + PricePulp_county +L_BioTot +BioPulpLD+postGrowthp+postGrowth_sqrp+PulpMills+AVG_val_ac+Distance.national_highway.+County+Conservation_Type +ELEV+Year+Coastal, data =Data_Clean_V2,na.action = na.exclude )

stargazer(P, title="pulpwood Combined", align=TRUE, dep.var.labels=c("Thinning","Final"),
          no.space=TRUE, type="text", out="Pulplog.htm")


#### Validation
## Deviance, log-likelihood and AIC
deviance(P)
logLik(P)
AIC(P)
## Log-likelihoods for full model and 0-model without predictors ##
S0<-multinom(ChoiceSawlog0  ~ 1, data =Data_Clean_V2,na.action = na.exclude)
LLf<-logLik(S)
LL0<-logLik(S0)
P0<-multinom(ChoicePulp0  ~ 1, data =Data_Clean_V2,na.action = na.exclude)
LLf<-logLik(P)
LL0<-logLik(P0)
## McFadden pseudo-R2
as.vector(1 - (LLf / LL0))
