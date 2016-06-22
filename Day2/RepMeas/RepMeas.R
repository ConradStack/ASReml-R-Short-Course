###############################
## Repeated Measures Analysis
###############################

rm(list=ls()) 
setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day2/RepMeas/")
library(asreml)
library(nadiv)

# Reading data
repcol<-read.table("REPCOLS.txt",h=T,na.string="*")
head(repcol)
repcol$Rep<-as.factor(repcol$Rep)
repcol$Timef<-as.factor(repcol$Time)   # Time as a factor
repcol$Timec<-repcol$Time              # Tiem as a variate
repcol$Female<-as.factor(repcol$Female)
repcol$Indiv<-as.factor(repcol$Indiv)
str(repcol)

# Performing some EDA
boxplot(HT~Timef,data=repcol)
aggregate(HT~Timef,mean,data=repcol)
aggregate(HT~Timef,sd,data=repcol)
table(repcol$Timef,repcol$Female)
meanst<-aggregate(HT~Female+Timef,mean,data=repcol)
interaction.plot(meanst$Timef,meanst$Female,meanst$HT)

# TRADITIONAL REPEATEAD MEASURES

# Model for single time point
rmodel1<-asreml(fixed=HT~Rep,random=~Female,
	              subset=Timef==4,data=repcol)
plot(rmodel1)
summary(rmodel1)$varcomp
(h2<-nadiv:::pin(rmodel1,h2~4*V1/(V1+V2)))

# Fitting model with AR1 Error Structure - No transformation
inite<-c(0.1,1,1,1,1)    # Very dummy
allmodel1<-asreml(fixed=HT~Rep+Timef+Timef:Rep,
	               random=~Female+Female:Timef,
						     rcov=~Indiv:ar1h(Timef,init=inite),data=repcol)
allmodel1<-update.asreml(allmodel1)   # Sometimes it works
plot(allmodel1)
summary(allmodel1)$varcomp

# Some predictions
predFem<-predict(allmodel1,classify="Female",sed=FALSE)$predictions$pvals
head(predFem)
predFemTime<-predict(allmodel1,classify="Female:Timef",sed=FALSE)$predictions$pvals
head(predFemTime)

# Calculating Genetic parameters
(h2<-nadiv:::pin(allmodel1,h2~4*V1/(V1+V2+V4+(V5+V6+V7+V8)/4)))
(rt2<-nadiv:::pin(allmodel1,rt2~V1/(V1+V2)))

# TRANSFORMING data and some EDA
repcol$logHT<-log(repcol$HT+1)*100
boxplot(logHT~Timef,data=repcol)
aggregate(logHT~Timef,mean,data=repcol)
aggregate(logHT~Timef,sd,data=repcol)

# Fitting model with AR1 Error Structure - with TRANSFORMATION
inite<-c(0.1,1,1,1,1)    # Very dummy
allmodel2<-asreml(fixed=logHT~Rep+Timef+Timef:Rep,
                  random=~Female+Female:Timef,
                  rcov=~Indiv:ar1h(Timef,init=inite),data=repcol)
allmodel2<-update.asreml(allmodel2)   # Sometimes it works
plot(allmodel2)
summary(allmodel2)$varcomp

# Calculating Genetic parameters
(h2<-nadiv:::pin(allmodel2,h2~4*V1/(V1+V2+V4+(V5+V6+V7+V8)/4)))
(rt2<-nadiv:::pin(allmodel2,rt2~V1/(V1+V2)))


# RANDOM REGRESSION APPROACH

# Changing to Random Regression (uncorrelated random terms)
inite<-c(0.1,1,1,1,1)
rreg1<-asreml(fixed=logHT~Rep+Timec+Timec:Rep,
	               random=~Female+Female:Timec,
						     rcov=~Indiv:ar1h(Timef,init=inite),data=repcol)
rreg1<-update.asreml(rreg1)   # Sometimes it works
plot(rreg1)
summary(rreg1)$varcomp
anova(rreg1,denDF='default')

# Random Regression (correlated random terms) 
inite<-c(0.85,863,1625,1341,465)
rreg2<-asreml(fixed=logHT~Rep+Timec+Timec:Rep,
              random=~str(~Female/Timec,~us(2,init=c(30,-1,1)):id(26)),
              rcov=~Indiv:ar1h(Timef,init=inite),data=repcol)
summary(rreg2)$varcomp
anova(rreg1,denDF='default')

# Some results
predFem<-predict(rreg2,classify="Female",sed=FALSE)$predictions$pvals
head(predFem)
predFemTimec<-predict(rreg2,classify="Female:Timec",levels=list(Timec=4),sed=FALSE)$predictions$pvals
View(predFemTimec)
rreg2$coefficients$fixed
rreg2$coefficients$random