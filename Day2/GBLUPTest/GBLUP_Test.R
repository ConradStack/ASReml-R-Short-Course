###################################
## Genomic Selection: GBLUP - Test
##################################

rm(list=ls()) 
#setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami")
library(asreml)

# Reading data
datag<-read.table("./Distribute/Day2/GBLUPTest/DATAG.txt",h=T)
head(datag)
dummyped<-read.table("./Distribute/Day2/GBLUPTest/DUMMYPED.txt",h=T)
head(dummyped)

# Creating Factors
head(datag)
datag$Indiv<-as.factor(datag$Indiv)
datag$Sire<-as.factor(datag$Sire)
datag$Dam<-as.factor(datag$Dam)
str(datag)

# Obtain the relatioship matrix - from dummy pedigree
ainvsire<-asreml.Ainverse(dummyped)$ginv
ainvsire
attr(ainvsire,"rowNames")

# Reading Ginverse (4 indivs) and assigning attr
GINV4<-read.table("./Distribute/Day2/GBLUPTest/GINVM4.txt",h=T)
gimatrix4<-data.frame(GINV4)
gimatrix4
attr(gimatrix4,"rowNames")<-c("10","20","30","40")
attr(gimatrix4,"rowNames")<-attr(ainvsire,"rowNames")

# Performing GBLUP (4)
mGBLUP1<-asreml(fixed=Resp~1,
                random=~giv(Sire)+Dam,
	              ginverse=list(Sire=gimatrix4),data=datag)
summary(mGBLUP1)$varcomp
(pred1<-predict(mGBLUP1,classify="Sire",sed=T)$predictions$pvals)


