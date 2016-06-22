###############################
## Alfalfa Experiment 
###############################

rm(list=ls())  # Removes all variables in memory
setwd("./Distribute/Day1/Alfalfa/")

alfalfa<-read.table("ALFALFA.TXT",header=TRUE)
head(alfalfa)  
summary(alfalfa)
str(alfalfa)

# Creating factors
str(alfalfa)
alfalfa$Variety<-as.factor(alfalfa$Variety)
alfalfa$Source<-as.factor(alfalfa$Source)
alfalfa$Block<-as.factor(alfalfa$Block)
str(alfalfa)

library(asreml)

# Full Model
model1<-asreml(fixed=Resp~Block,random=~Variety,data=alfalfa)
summary(model1)
log1<-model1$loglik

# Restricted Model
model0<-asreml(fixed=Resp~Block,data=alfalfa)
summary(model0)
log0<-model0$loglik
