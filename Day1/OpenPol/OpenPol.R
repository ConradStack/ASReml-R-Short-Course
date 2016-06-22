###############################
## Sire Model (Female) 
###############################

rm(list=ls()) 
setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day1/OpenPol")
library(asreml)
library(ggplot2)

# Open Pollinated Example
openp<-read.table("OPENPOL.txt", h=T)
head(openp)
openp$REP<-as.factor(openp$REP)
openp$FEMALE<-as.factor(openp$FEMALE)
openp$PLOT<-as.factor(openp$PLOT)
openp$TYPE<-as.factor(openp$TYPE)
str(openp)











# Looking at some of the predictions
ggplot(preds,aes(x=FEMALE,y=predicted.value,fill=FEMALE)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=predicted.value-standard.error, ymax=predicted.value+standard.error),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9))

