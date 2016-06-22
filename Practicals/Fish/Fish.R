###############################
## Animal Model
###############################

rm(list=ls()) 
setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day1/Fish")
library(asreml)
library(nadiv)

# Fish Example 
fish<-read.table("FISHAB.txt", h=T)
head(fish)
fish$Sex<-as.factor(fish$Sex)
fish$INDIV<-as.factor(fish$INDIV)
fish$Sire<-as.factor(fish$Sire)
fish$Dam<-as.factor(fish$Dam)
fish$FAM<-as.factor(fish$FAM)
str(fish)

#########
# Part 1 - PARENTAL MODEL  WITH  PEDIGREE
pedpar<-read.table("PEDPAR.txt",h=T)
ainvpar<-asreml.Ainverse(pedpar)$ginv

# Fitting a Parental model
parentalmodel<-asreml(fixed=DaysM~Sex,
	                    random=~ped(Sire)+and(ped(Dam))+FAM,
	                    ginverse=list(Sire=ainvpar,Dam=ainvpar),
	                    data=fish,workspace=64e06)
plot(parentalmodel)
summary(parentalmodel)$varcomp
pedparentalmodel<-predict(parentalmodel,classify="Sire", sed=TRUE) 
View(pedparentalmodel$predictions$pvals)

# Genetic Variances
(h2<-nadiv:::pin(parentalmodel,h2~4*V1/(2*V1+V2+V3)))
(d2<-nadiv:::pin(parentalmodel,d2~4*V2/(2*V1+V2+V3)))


#########
# Part 2 - INDIVIDUAL MODEL WITH PEDIGREE
pedind<-read.table("PEDIND.txt",h=T)
ainvind<-asreml.Ainverse(pedind)$ginv

# Fitting individual model
Indvmodel<-asreml(fixed=DaysM~Sex,
	                random=~ped(INDIV)+FAM,
	                ginverse=list(INDIV=ainvind),data=fish)
summary(Indvmodel)$varcomp
plot(Indvmodel)
pedIndvmodel<-predict(Indvmodel,classify="INDIV", sed=TRUE)
View(pedIndvmodel$predictions$pvals)

# Genetic Variances
(h2<-nadiv:::pin(Indvmodel,h2~V1/(V1+V2+V3)))
(d2<-nadiv:::pin(Indvmodel,d2~4*V2/(V1+V2+V3)))

# Model No-family
IndvmodelNF<-asreml(fixed=DaysM~Sex,
                  random=~ped(INDIV),
                  ginverse=list(INDIV=ainvind),data=fish)
summary(IndvmodelNF)$varcomp

# Using asremlplus
library(asremlPlus)
reml.lrt.asreml(Indvmodel,IndvmodelNF,positive.zero=TRUE)  # LRT with pvalues
info.crit.asreml(Indvmodel)     # AIC and BIC - the larger the better
info.crit.asreml(IndvmodelNF) 
