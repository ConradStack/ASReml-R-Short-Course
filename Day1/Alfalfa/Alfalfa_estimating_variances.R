
library(nadiv)
library(asremlPlus)
library(asreml)

# rm(list=ls())  # Removes all variables in memory
# setwd("./Distribute/Day1/Alfalfa/")
alfalfa<-read.table("ALFALFA.TXT",header=TRUE)


# Creating factors
str(alfalfa)
alfalfa$Variety<-as.factor(alfalfa$Variety)
alfalfa$Source<-as.factor(alfalfa$Source)
alfalfa$Block<-as.factor(alfalfa$Block)
str(alfalfa)

# Fitting asreml model
model1 <- asreml(fixed=Resp ~ Block, random= ~Variety, data=alfalfa)
summary(model1)

# H2 = var(Variety) / (var(Variety) + var(Error))
H2 <- nadiv:::pin(model1, tmp ~ V1 / (V1 + V2))
# NB -> the standard error is not *exactly* correct: the heritability estimate will not necessarily be normally distributed (they only will if a very large number of samples are available.)

# Likelihood ratio test (Full vs restricted model)
# NB -> the below is equivalent to testing whether heritability is > 0
model0 <- asreml(fixed=Resp ~ Block, data=alfalfa)
reml.lrt.asreml(full.asreml.obj = model1, model0, positive.zero = TRUE) 

# Get the AIC and BIC of both models
# (smaller is better)
info.crit.asreml(model1) 
info.crit.asreml(model0)





