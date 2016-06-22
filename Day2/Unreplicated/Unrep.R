#########################################
# Spatial Analysis - Unreplicated Trial #
#########################################

rm(list=ls()) 
setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day2/Unreplicated")
library(asreml)
library(nadiv)
library(asremlPlus)

unrep<-read.table("PEPPER.txt",h=T)
unrep$Rep<-as.factor(unrep$Rep)
unrep$Xf<-as.factor(unrep$X)
unrep$Yf<-as.factor(unrep$Y)
unrep$Genotype<-as.factor(unrep$Genotype)
head(unrep)
View(unrep)

# Some EDA
plot(unrep$X,unrep$Y)
table(unrep$Rep,unrep$Genotype)

# Traditional Augmented analysis - No spatial!!
nospatial<-asreml(fixed=YD~1,
                  random=~Rep+Genotype,
                  rcov=~Xf:Yf,data=unrep)
summary(nospatial)$varcomp
plot(nospatial)
plot(variogram(nospatial))
(rhos<-nadiv:::pin(nospatial,rhos~V1/(V1+V2+V3)))
(H2<-nadiv:::pin(nospatial,H2~V2/(V1+V2+V3)))
predtrad<-predict(nospatial,classify="Genotype")$predictions$pvals
View(predtrad)

# Basic Spatial Analysis for Augmented Designs

