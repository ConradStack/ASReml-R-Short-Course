###############################
## Genomic Selection: GBLUP
###############################

rm(list=ls()) 
#setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami")
library(asreml)
library(nadiv)

# Reading data
datag<-read.table("./Distribute/Day2/GBLUPFull/Clonaldata.txt",h=T,na.strings='.')
ped<-read.table("./Distribute/Day2/GBLUPFull/pedgenot.txt",h=T)
peddummy<-read.table("./Distribute/Day2/GBLUPFull/peddummy.txt",h=T)

#datag<-datag[with(datag,order(datag$clone)), ]  # Sorting by clone

# Creating Factors (if necessary)
head(datag)
datag$Rep<-as.factor(datag$Rep)
datag$Block<-as.factor(datag$Block)
datag$Row<-as.factor(datag$Row)
datag$Col<-as.factor(datag$Col)
str(datag)

# TRADITIONAL ANALYSIS

# Fitting a simple animal model - using pedigree file
head(ped)
ainv<-asreml.Ainverse(ped)$ginv
head(ainv)

# Fiting simple model
