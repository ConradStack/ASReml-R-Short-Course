###############################
## Animal Model
###############################

rm(list=ls()) 
library(asreml)

# Fish Example 
fish<-read.table("./Distribute/Day1/Fish/FISHAB.txt", header=T)
head(fish)
fish$Sex<-as.factor(fish$Sex)
fish$INDIV<-as.factor(fish$INDIV)
fish$Sire<-as.factor(fish$Sire)
fish$Dam<-as.factor(fish$Dam)
fish$FAM<-as.factor(fish$FAM)
str(fish)

#########
# Part 1 - PARENTAL MODEL  WITH  PEDIGREE
pedpar<-read.table("./Distribute/Day1/Fish/PEDPAR.txt",h=T)
ainvpar<-asreml.Ainverse(pedpar)$ginv

# Fitting a Parental model
parentalmodel<-asreml(fixed=DaysM~Sex,
	                    random=~ped(Sire)+and(ped(Dam))+FAM,
	                    ginverse=list(Sire=ainvpar,Dam=ainvpar),
	                    data=fish,workspace=64e06)
plot(parentalmodel)


#########
# Part 2 - Individual MODEL  WITH  PEDIGREE
pedind<-read.table("./Distribute/Day1/Fish/PEDIND.txt",h=T)
ainv<-asreml.Ainverse(pedind)$ginv

model2<-asreml(fixed=DaysM~Sex,
							 random=~ped(INDIV)+FAM,
							 ginverse=list(INDIV=ainv),
							 data=fish,workspace=64e06)

plot(model2)

## Compare parental and individual models:
summary(parentalmodel)$varcomp
summary(model2)$varcomp

summary(parentalmodel)$varcomp
(h2 <- nadiv:::pin(parentalmodel, tmp ~ (4*V1) / (2*V1 + V2 + V3) ) )
blup <- summary(parentalmodel,all=TRUE)$coef.random

summary(model2)$varcomp
(h2 <- nadiv:::pin(model2, tmp ~ (V2) / (V1 + V2 + V3) ) )
blup2 <- summary(model2,all=TRUE)$coef.random

# NB -> see how the individual ped variance component is 4 times larger than the parental model.
# NB -> the residual variance of the parental model is larger BECAUSE it incorporates 3/4 of the additive genetic variance.  This is very important point!  It relates back to how the heritability and genetic variance components are derived.


head(blup);head(blup2)
head( blup[grepl("^ped",rownames(blup)),] )
head( blup2[grepl("^ped",rownames(blup2)),] )

head( blup[grepl("^FAM",rownames(blup)),] )
head( blup2[grepl("^FAM",rownames(blup2)),] )

