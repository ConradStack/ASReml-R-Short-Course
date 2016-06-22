# Practical 1

rm(list=ls())  # Removes all variables in memory
setwd("Distribute/Practicals/FieldT")

grape<-read.table("FIELDT.TXT",header=TRUE,na.strings="-9.00")
head(grape)  
View(grape)

library(asreml)
# y = mu + REP + REP:IBLOCK + REP:IBLOCK:PLOT + GENOTYPE + e

grape$REP<-as.factor(grape$REP)
grape$IBLOCK<-as.factor(grape$IBLOCK)
grape$PLOT<-as.factor(grape$PLOT)
grape$GENOTYPE<-as.factor(grape$GENOTYPE)
str(grape)
#grape$YD[144]<-NA
#grape$YD[217]<-NA
#grape$YD[250]<-NA

## Interpretations:
# REP:IBLOCK - IBLOCK within REP
# REP:IBLOCK:PLOT - PLOT within IBLOCK within REP
#
model1 <- asreml(YD ~ REP, 
					  random = ~ REP:IBLOCK + REP:IBLOCK:PLOT + GENOTYPE, 
					  data = grape)

hist(grape$YD)
View(grape)


model1<-asreml(fixed=YD~REP,
	             random=~REP:IBLOCK+REP:IBLOCK:PLOT+GENOTYPE,data=grape)
plot(model1)
summary(model1)
summary(model1)$varcomp
Num<-summary(model1)$varcomp$component[3]
Den<-sum(summary(model1)$varcomp$component)
(H2<-Num/Den)

wald(model1)
wald(model1,denf='defualt',ssType='incremental')
wald(model1,denf='defualt',ssType='conditional')



