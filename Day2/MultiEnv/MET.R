###############################
## Multi-Environment Trials
###############################

rm(list=ls()) 
#setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day2/MultiEnv")
library(asreml)
#library(nadiv)

# Reading data
datam<-read.table("./Distribute/Day2/MultiEnv/TRIALS4.txt",h=T)
head(datam)
datam$Rep<-as.factor(datam$Rep)
datam$Iblock<-as.factor(datam$Iblock)
datam$Test<-as.factor(datam$Test)
datam$Genotype<-as.factor(datam$Genotype)
str(datam)

# Performing some EDA
boxplot(HT~Test,data=datam)
aggregate(HT~Test,mean,data=datam)
table(datam$Test,datam$Genotype)
meanst<-aggregate(HT~Genotype+Test,mean,data=datam)
interaction.plot(meanst$Test,meanst$Genotype,meanst$HT)


# Model for a single test
model1<-asreml(fixed=HT~Rep,
	             random=~Rep:Iblock+Genotype,data=datam,
	             subset=Test==2)
summary(model1)$varcomp
plot(model1)
(h2b<-nadiv:::pin(model1,h2b~4*V1/(V1+V2+V3)))
# heritability is biased when you calculate it for a single site



### Explicit Model #

# Model for a single test
model1all<-asreml(fixed=HT~ Test + Test:Rep,
					random= ~ at(Test):Rep:Iblock+Genotype + Test:Genotype,
					data=datam)
summary(model1all)$varcomp
plot(model1all)
(h2b<-nadiv:::pin(model1all,h2b~4*V1/(V1+V2+V3)))


# Simple Model for all sites #
# NB -> the at(Test, [1-4]) calls allow you to specify different random effects for the different sites.  You might use this, for example, if 3 trials are incomplete blocks and 1 isnt'
model2<-asreml(fixed=HT~Test+Test:Rep,
               random=~at(Test,1):Rep:Iblock+at(Test,2):Rep:Iblock+
                      at(Test,3):Rep:Iblock+at(Test,4):Rep:Iblock+
                      Genotype+Test:Genotype,
               rcov=~units:at(Test),data=datam)
summary(model2)$varcomp
plot(model2)
(r2B<-nadiv:::pin(model2,r2B~V5/(V5+V6)))

# Some predictions
ppG2<-predict(model2,classify="Genotype")
View(ppG2$predictions)
View(model2$coefficient$random)
ppGE2<-predict(model2,classify="Testf:Genotype")
View(ppGE2$predictions)

### Implicit Model ###

# Simple corv similar to Explicit
initg<-c(0.65,400)
model2b<-asreml(fixed=HT~Test+Test:Rep,
               random=~at(Test):Rep:Iblock+
                      corv(Test,init=initg):Genotype,
                rcov=~at(Test):units,data=datam)
summary(model2b)$varcomp

# US model
initg<-c(520.7,392.2,563.6,256.7,376.6,392.1,384.1,268.8,200.0,356.8)
initg = sample(1000:5000,10)/10

model3<-asreml(fixed=HT~Test+Test:Rep,
               random=~at(Test):Rep:Iblock+
                      us(Test,init=initg):Genotype,
               rcov=~at(Test):units,data=datam)
model3<-update.asreml(model3)
summary(model3)$varcomp


initg = c( sample(5:95,6) / 100,
	c(sample(1000:5000,4)/10)
)

model4<-asreml(fixed=HT~Test+Test:Rep,
					random=~at(Test):Rep:Iblock+
						corgh(Test,init=initg):Genotype,
					rcov=~at(Test):units,data=datam)
model4<-update.asreml(model4)
summary(model4)$varcomp


# LRT
require(asremlPlus)
reml.lrt.asreml(model3,model2b,positive.zero = FALSE)  # if you are testing covariances or correlations, then positive.zero must be FALSE.  If all variances, then set to TRUE
info.crit.asreml(model2b)
info.crit.asreml(model3)

reml.lrt.asreml(model4,model2b,positive.zero = FALSE)  # if you are testing covariances


