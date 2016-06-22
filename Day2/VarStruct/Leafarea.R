###############################
## Variance Structure
###############################

rm(list=ls()) 
#setwd("E:/WORK_PARTIAL/ASReml/ASReml_2016_Miami/Distribute/Day2/VarStruct")
library(asreml)


# Leafarea Example
leafarea<-read.table("./Distribute/Day2/VarStruct/LEAFAREA.txt", h=T)
head(leafarea)
leafarea$block<-as.factor(leafarea$block)
leafarea$pot<-as.factor(leafarea$pot)
leafarea$variety<-as.factor(leafarea$variety)
leafarea$disease<-as.factor(leafarea$diseas)

# Some EDA
hist(leafarea$leafarea)   # Transformation?


# Initial Analysis - All fixed!
model0<-asreml(fixed=leafarea~block+disease+variety+variety:disease,
               data=leafarea)
plot(model0)
anova(model0,denDF='default')
pred.model0<-predict(model0,classify="variety:disease",sed=TRUE)
View(pred.model0$predictions$pvals)
# NB -> the residuals look very skewed (not good).  This model is assuming that all observations have the same background noise!  This is clearly not correct, so move to reml analysis where we can assign different variances to variety and variety:disease error separately.  
# NB -> if we want to select varieties, then we will (probably?) need to use reml model (below)


# Initial Analysis - Variety Random!
model1<-asreml(fixed=leafarea~block+disease,
               random=~variety+variety:disease,
               data=leafarea)
plot(model1)
summary(model1)$varcomp
anova(model1,denDF='default')


#### DIRECT SUM ####
# NB -> units is a keyword in asreml-r
# NB -> rcov = ~units by itself is basically an indentity
# NB -> rcov = ~ at(disease):units ; this command does a direct sum of indentity matrix (units) and different levels of disease
#
leafarea = leafarea[order(leafarea$disease),]
model1b<-asreml(fixed=leafarea~block+disease,
					random=~variety+variety:disease,
					rcov = ~units:at(disease),   # error variance, one for each disease state
					data=leafarea)

plot(model1b)
summary(model1b)$varcomp
wald(model1b, denDF="default")  # the anova: gauging significance (not 0) of the fixed effects.
rhoB <- 41.98 / (41.98+173.38)
rhoB <- nadiv:::pin(model1b, tmp ~ V1 / (V1 + V2) )  # indicates whether certain plants, G, do better in certain environments, GxE.  If this correlation is near 0, then GxE is strong and we can only assume particular plants do well in particular environments.  This case would be bad for breeding programs!
# NB -> (extremely important!!!)  The raw residuals are on different scales!  (we need studentized residuals)
# Note how the variances are VERY different btw the different disease states!  This is critical to understand.  Each site should have its own error variance.
# Exploratory data analysis (EDA) should help to indicate how to get to model1b, where we are modeling different error variances for the different disease states. 
# Transforming the data is to make your life easier.  Its a trick, makes model easier, but possibly interpretation more difficult.
# NB -> Genetic Variance, G == variety!variety.var 
# NB -> GxE Variance == variety:disease!variety.var


#### DIRECT PRODUCT ####
# NB -> this will be a GxE analysis
# NB -> id() says to use an identify matrix for variety
# NB -> us() says to use an unstructured matrix for disease
model1c<-asreml(fixed=leafarea~block+disease,
					 random=~id(variety):us(disease),  # This defines the structure of the G matrix (variety:disease interaction) == I_12 (kron) 2x2 matrix of disease variances
					 rcov = ~units:at(disease),   # error variance, one for each disease state
					 data=leafarea)
#plot(model1c)
summary(model1c)$varcomp
wald(model1c, denDF="default")  # the anova: gauging significance (not 0) of the fixed effects.
(rhoB <- nadiv:::pin(model1c, tmp ~ V2 / sqrt(V1 * V3) )  )  # type B correlation


# using a different variance structure, which is estimating the correlation directly, instead of 3 variance components.  
# NB -> this is the same model as 1c, just a different parameterization!
model1d<-asreml(fixed=leafarea~block+disease,
					 random=~id(variety):corv(disease),  
					 rcov = ~units:at(disease),   # error variance, one for each disease state
					 data=leafarea)
summary(model1d)$varcomp



