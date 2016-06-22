###############################
## Clonal Model
###############################

rm(list=ls()) 
setwd("./Distribute/Day1/Clonal/")
library(asreml)
library(nadiv)
library(plyr)

# Reading Files and generationg AINVERSE
clonal<-read.table("CLONES.txt",h=T)
pedpar<-read.table("PEDPAR.txt",h=T)
ainv<-asreml.Ainverse(pedpar)$ginv

require(igraph)
gr = graph.edgelist(unique(as.matrix(clonal[,c("Female","Male")])))
plot(gr)
is.connected(gr,"weak")

head(clonal)
head(pedpar)
clonal$Rep<-as.factor(clonal$Rep)  
clonal$IncBlock<-as.factor(clonal$IncBlock)
clonal$Female<-as.factor(clonal$Female)
clonal$Male<-as.factor(clonal$Male)
clonal$FamilyID<-as.factor(clonal$FamilyID)
clonal$cloneid<-as.factor(clonal$cloneid)
str(clonal)

# Fitting Simple Clonal Model - no pedigree
model1 = asreml(VOL ~ Rep,
					 random = ~ Rep:IncBlock + Female + Male + FamilyID + cloneid, data=clonal)

BLUP = model1$coefficients$random
BLUP = BLUP[grepl("^cloneid",names(BLUP))]

pred.clones = predict(model1, classify="cloneid")
pred.clones$predictions$pvals


# Fitting Full Clonal Model - with pedigree
model2 = asreml(VOL ~ Rep,
					 random = ~ Rep:IncBlock + ped(Female) + ped(Male) + FamilyID + cloneid, 
					 ginverse=list(Male=ainv,Female = ainv),
					 data=clonal)
plot(model2)

# Overlay parental effects (using the "and(...)" function, which says to incorporate the female and male effects together using the pedigree)
model3 = asreml(VOL ~ Rep,
					 random = ~ Rep:IncBlock + ped(Female) + and(ped(Male)) + FamilyID + cloneid, 
					 ginverse=list(Male=ainv,Female = ainv),
					 data=clonal)

# NB -> there is often a slight positive correlation between fitted values and residuals when using the pedigree.  why?


BLUP = summary(model3, all=TRUE)$coef.random
# clonal effect is everything left over after removing male + female + family effects (i.e., they are not directly comparable...)

# Genetic components
summary(model3)$varcomp
(h2 <- nadiv:::pin(model3, tmp ~ (4*V1) / (2*V1 + V2 + V3 + V4 + V5) ) )
(d2 <- nadiv:::pin(model3, tmp ~ (4*V2) / (2*V1 + V2 + V3 + V4 + V5) ) )
(i2 <- nadiv:::pin(model3, tmp ~ (V4 - 2*V1 - 3*V2) / (2*V1 + V2 + V3 + V4 + V5) ) )
(H2 <- nadiv:::pin(model3, tmp ~ (2*V1 + V2 + V4 ) / (2*V1 + V2 + V3 + V4 + V5) ) )


# Easy model - ignoring everything but cloneid
model4 = asreml(VOL ~ Rep,
					 random = ~ Rep:IncBlock + cloneid, 
					 data=clonal)
preds = predict(model4,classify="cloneid")$prediction$pvals
summary(model4)$varcomp
(H2 <- nadiv:::pin(model4, tmp ~ (V2) / (V1 + V2 + V3) ) )
# NB -> this one is handy if you want to rank clones only.  However, if you want to estimate anything from the parents or understand the breakdown of the genetic variance components


