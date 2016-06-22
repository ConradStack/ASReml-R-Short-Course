###############################
## Repeated Measures as MV
###############################

rm(list=ls()) 
#setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami")
library(asreml)
#library(nadiv)

# Reading and Preparing the Data
mvcol<-read.table("./Distribute/Day2/RepMeas/MVCOLS.txt",h=T,na.string="*")
head(mvcol)
mvcol$Rep<-as.factor(mvcol$Rep)
mvcol$Female<-as.factor(mvcol$Female)
mvcol$Indiv<-as.factor(mvcol$Indiv)
str(mvcol)

# Model for single time point
rmodel1<-asreml(fixed=HT4~Rep,random=~Female,data=mvcol)
plot(rmodel1)
summary(rmodel1)$varcomp
(h2<-nadiv:::pin(rmodel1,h2~4*V1/(V1+V2)))
# (TODO) Check for outliers, get initial values for variance components, etc


# MV 2 - Starting with a bivariate model
initf<-c(0.1,36,74)  # again, the variance element here come from the univariate model
inite<-c(0.1,419,1405)
mvmodel2<-asreml(fixed= cbind(HT1,HT2) ~ trait + trait:Rep,
	             random= ~corh(trait,init=initf):Female,
               rcov=~units:corh(trait,init=inite),data=mvcol)
summary(mvmodel2)$varcomp
# (TODO) Check for outliers, get initial values for variance components, etc


# MV 4 - More challenging model but still corh (maybe change to coruh)
initf<-c(0.8,36,74,117,210)
inite<-c(0.8,419,1405,3801,5154)
mvmodel4<-asreml(fixed=cbind(HT1,HT2,HT3,HT4)~trait+trait:Rep,
                 random=~corh(trait,init=initf):Female,
                 rcov=~units:corh(trait,init=inite),data=mvcol)
summary(mvmodel4)$varcomp
# NB -> corh(trait,init=initf):Female is the "genetic correlation" between the traits.  It is conditioning

predf = predict(mvmodel4, classify="trait:Female")$predictions$pvals

# HW -> replace corh with corgh, starting with trait:Female
initf<-c( sample(seq(0.5,0.9,by=0.05), 6) 
			 ,36,74,117,210)
inite<-c( 0.8 ,419,1405,3801,5154)
mvmodel5 <- asreml(fixed = cbind(HT1,HT2,HT3,HT4) ~ trait + trait:Rep,
					  random = ~corgh(trait,init=initf):Female,
					  rcov = ~units:corh(trait,init=inite),data=mvcol)
#summary(mvmodel5)$varcomp
