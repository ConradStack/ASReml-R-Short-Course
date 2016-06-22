###############################
## Alfalfa Experiment 
###############################

rm(list=ls())  # Removes all variables in memory
setwd("./Distribute/Day1/Alfalfa/")

alfalfa<-read.table("ALFALFA.TXT",header=TRUE)
head(alfalfa)  
summary(alfalfa)
str(alfalfa)

# Boxplot for Each variety and general histogram
boxplot(Resp~Variety,data=alfalfa)
hist(alfalfa$Resp, main='Alfalfa Experiment',xlab='Yield')

# Creating factors
str(alfalfa)
alfalfa$Variety<-as.factor(alfalfa$Variety)
alfalfa$Source<-as.factor(alfalfa$Source)
alfalfa$Block<-as.factor(alfalfa$Block)
str(alfalfa)

# Fitting model with blocks fixed
alflm<-lm(Resp~Block+Variety,data=alfalfa)
summary(alflm)
anova(alflm)


# Analysis using ASReml

library(asreml)

# asreml(fixed=y~<fixed effects>,
#        random=~<random effects>.
#        rcov=<error structure>)
model1 <- asreml(fixed=Resp ~ Block, random= ~Variety, data=alfalfa)

# the variance component estimates.  
# NB -> the conf intervals (z-ratio) are calculated assuming infinite df, this is NOT valid do not use these!
summary(model1)$varcomp   	

# Wald method to calculate the variance of the variance components
wald(model1,denDF="default")   # 

# BLUPs and BLUEs
ls(model1)  # everything available within model1 object (cool!)
model1$coefficients$fixed   # BLUEs  
model1$coefficients$random  # BLUPs

# Another way to get the BLUPs (with standard errors)
# NB -> the z ratio again is not valid (it is based on approximated SE and assumes an infinite degrees of freedom).  
BLUP <- (summary(model1, all=TRUE)$coef.random) 


# Predictions (asreml) 
predictions <- predict(model1, classify="Variety")  # same as asking for lsmeans
predictions$predictions$pvals   # incorporate BLUPs and fixed effects
# mean of a fixed effect is always zero (Salvador)
# prediction is = mu + (mean of Block effects == 0) + V_a (the BLUP value for each variety)


# Using Block as a random effect along with Variety
model2 <- asreml(fixed=Resp ~ 1, random= ~ Block + Variety, data=alfalfa)
BLUP2 <- (summary(model2, all=TRUE)$coef.random) 
pred.block <- predict(model2, classify="Block")  # not meaningful / interesting
pred.block$predictions$pvals
pred.variety <- predict(model2, classify="Variety")
pred.variety$predictions$pvals

# Using blocks as random effects can only increase the SE's of the predictions (of each variety).  There appears to be substantial variety among blocks, incorporating this information is importance for prediction
# 







