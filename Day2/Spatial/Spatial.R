#####################################
##  Spatial Analysis  - Replicated ##
#####################################

rm(list=ls()) 
setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day2/Spatial")
library(asreml)
library(nadiv)
library(asremlPlus)

spatial<-read.table("ROWCOL.txt",h=T)
head(spatial)
spatial$ROW<-as.factor(spatial$ROW)
spatial$COL<-as.factor(spatial$COL)
spatial$REP<-as.factor(spatial$REP)
spatial$REP.PLOT<-as.factor(paste(spatial$REP,spatial$PLOT,sep="."))
spatial$Xf<-as.factor(spatial$X)    # X coordinate as a factor
spatial$Yf<-as.factor(spatial$Y)    # Y coordinate as a factor
spatial$FEMALE<-as.factor(spatial$FEMALE)
head(spatial)
str(spatial)

# Some EDA for checking grid
plot(spatial$X,spatial$Y)

# Basic Model without spatial components but design components #
nospatial<-asreml(fixed=YA~REP,
                  random=~REP:ROW+REP:COL+REP.PLOT+FEMALE,
                  data=spatial)
summary(nospatial)$varcomp
plot(nospatial)
(nospatial$loglik)

# Obtaining Variogram of basic nospatial model
nospatial<-asreml(fixed=YA~REP,
                  random=~REP:ROW+REP:COL+REP.PLOT+FEMALE,
                  rcov=~id(Xf):id(Yf),data=spatial)
plot(variogram(nospatial))

# Incorporating Spatial Autocorrelation on X and Y
spatial1<-asreml(fixed=YA~REP,
                 random=~REP:ROW+REP:COL+REP.PLOT+FEMALE,
	               rcov=~ar1(Xf):ar1(Yf),data=spatial)
summary(spatial1)$varcomp
plot(spatial1)
plot(variogram(spatial1))

# Using asremlplus
reml.lrt.asreml(spatial1,nospatial,positive.zero=FALSE)  # LRT with pvalues
info.crit.asreml(nospatial)                           # AIC and BIC
info.crit.asreml(spatial1)                           # AIC and BIC


# Playing with fixed effects: Adding global trend fixed effects #
spatial3<-asreml(fixed=YA~REP+X+Y,random=~REP:ROW+REP:COL+REP.PLOT+FEMALE,
	               rcov=~ar1(Xf):ar1(Yf),data=spatial)
plot(spatial3)
summary(spatial3)$varcomp
anova(spatial3,denDF='default')

# Selected model with only autocorrelation for X #
spatial4<-asreml(fixed=YA~REP+X+Y,
	               random=~REP:ROW+REP:COL+REP.PLOT+FEMALE,
	               rcov=~ar1(Xf,init=0.3):Yf,data=spatial)
anova(spatial4,denDF='default')
plot(variogram(spatial4))
summary(spatial4)$varcomp

# Some predictions
pred.nospatial<-predict(nospatial,classify="FEMALE",sed=TRUE)$predictions$pvals
head(pred.nospatial)
pred.spatial4<-predict(spatial4,classify="FEMALE",sed=TRUE)$predictions$pvals
head(pred.spatial4)

# Selected model with only autocorrelation for X -Incorporate NUGGET #
spatial4nugg<-asreml(fixed=YA~REP+X+Y,
	           random=~REP:ROW+REP:COL+REP.PLOT+FEMALE+units,
	           rcov=~ar1(Xf,init=0.3):ar1(Yf,init=0.3),data=spatial)
spatial4nugg<-update.asreml(spatial4nugg)
summary(spatial4nugg)$varcomp
plot(variogram(spatial4nugg))
