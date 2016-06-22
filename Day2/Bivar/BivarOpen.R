#################################
## Sire Model (Female) Bivariate
#################################

rm(list=ls()) 
#setwd("./Distribute/Day2/Bivar")
library(asreml)
library(nadiv)

# Open Pollinated Example
openp<-read.table("./Distribute/Day2/Bivar/OPENPOL.txt", h=T)
head(openp)
openp$REP<-as.factor(openp$REP)
openp$FEMALE<-as.factor(openp$FEMALE)
openp$PLOT<-as.factor(openp$PLOT)
openp$TYPE<-as.factor(openp$TYPE)
str(openp)

# Single Trait Analysis 
model.HT<-asreml(fixed=HT~REP,random=~FEMALE+PLOT,data=openp)
plot(model.HT)
summary(model.HT)$varcomp

model.DBH<-asreml(fixed=DBH~REP,random=~FEMALE+PLOT,data=openp)
plot(model.DBH)
summary(model.DBH)$varcomp

# (TODO) calculate heritability, clean outliers, etc....

## Bivariate Analysis ###
with(openp, cor(HT,DBH,use="na") )  # phenotypic correlation (not very useful for gauging indirect selection)

# Model to Start
# NB -> "trait" in the fixed effects indicates that each response variable (trait) will have its own intercept (mean).  So, basically, the two traits are fit with two different models ... except they are tied together 
### trait:REP -> "REP nested withing trait"
### us(trait,initf):FEMALE -> we want to gauge the correlation 
### diag(trait,init=initp):PLOT -> diag finds different variance for each level of plot
### id(...) is unnecessary (it is the default), but it is good to be more deliberate / explicit ... forces you to think about what you are actually modeling
### with 2+ traits requires a flexible rcov (error variance structure)
### -> everything is conditional on trait...
# NB -> **** starting values of variances can come from earlier, univariate analyses!!! ****
# NB -> In salvador's experience, corgh() works best for random effects and us() work best for error variances
initf<-c(0.19,0.31,0.69)  # starting values for un(structured) variance matrix for FEMALE; for corgh, start with correlations, then finish with variances (see ASReml workshop booklet as a reference for how to order these things).  
initp<-c(0.05,0.001)  # starting values for diag(onal) variance matrix for PLOT
inite<-c(1.02,1.82,7.27)  # starting values for un(structured) variance matrix for Residuals 
modelb1<-asreml(fixed = cbind(HT,DBH) ~ trait + trait:REP,
	     random = ~corgh(trait,initf):FEMALE + diag(trait,init=initp):PLOT, 
	     rcov = ~units:us(trait,init=inite),  # if units:at(trait) is used, then you are assuming that the observations have to separated because they don't / shouldn't share residual variance...
	     maxiter = 20,workspace=256e06,data=openp)
modelb1 <- update.asreml(modelb1)
# plot(modelb1)     # Does not work even if you try!
summary(modelb1)$varcomp

# NB -> why are NA's for std.error?  Because of numerical issues, where a variance is too close to 0 or 1.  Seeing this SHOULD feedback into your modeling (corgh -> diag , say, if correlation from corgh is close to 0 or 1)
# 
# NB -> You don't need to specify initial values, but it can be more convenient if you've already run a number of univariate analyses.   Also, the univariate analyses can tell you if a biariate analysis is even worthwhile (e.g., if one trait has an extremely low heritability, then it prob isn't worth trying to find the genetic correlation between it and another trait ... **** )
# NB -> If data are not "clean", and you see that results have not diverged from the initial values, then you might get worried that your inital value choices are having too much impact on your results... this can be checked by choosing different set of initial values
# NB -> Salvador said it is often easier to do many bivariate analyses if you can't get the multivariate model likelihood to converge
# NB -> the response variables need to be close to the same scale ( put on standard normal with unit variance scale. )
 

# Calculating Genetic parameters
library(nadiv)
(h2_1<-nadiv:::pin(modelb1,h2_1~4*V1/(V1+V4+V7)))
(h2_2<-nadiv:::pin(modelb1,h2_2~4*V3/(V3+V5+V9)))
(rA2<-nadiv:::pin(modelb1,rA2~V2/sqrt(V1*V3)))
(rP2<-nadiv:::pin(modelb1,rP2~(V2+0+V8)/sqrt((V1+V4+V7)*(V3+V5+V9))))
