###############################
## Full Sib Model 
###############################

rm(list=ls()) 
setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day1/ContPol")
library(asreml)

fs<-read.table("CONTPOL.txt", h=T)
head(fs)
fs$REP<-as.factor(fs$REP)
fs$FEMALE<-as.factor(fs$FEMALE)
fs$MALE<-as.factor(fs$MALE)
fs$FAMILY<-as.factor(fs$FAMILY)
fs$CHECKLOT<-as.factor(fs$CHECKLOT)
str(fs)

# Analysis Full-Sib - Two additive terms
FSIB<-asreml(fixed=YIELD~REP,random=~FEMALE+MALE+FEMALE:MALE,data=fs)
summary(FSIB)$varcomp
View(FSIB$coefficients$random) 
pred.MALE<-predict(FSIB,classify="FEMALE",sed=TRUE)
pred.FEMALE<-predict(FSIB,classify="FEMALE",sed=TRUE)
View(pred.FEMALE$predictions$pvals)

