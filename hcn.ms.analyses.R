#################################################################################
#################################################################################
################## Script for HCN Meter Manuscript -  ANALYSES ##################
#################################################################################
#################################################################################


# Colin Richard Morrison 
# PhD Candidate
# The University of Texas at Austin 
# Department of Integrative Biology 
# Graduate Program In Ecology, Evolution and Behavior
# crmorrison@utexas.edu


getwd()
setwd("~/Desktop/HCN")


### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
#### ----------- FIGURE 5 - Method Comparison 

comp=read.csv("method.comparison.csv", header=T)
colnames(comp)

### ---NORMALITY tests of Meter - Spectrophotometer Data
shapiro.test(comp$mpPAIR) # Shapiro-Wilks test for normality. 
# W = 0.86033, p-value = 0.0005794
# data not normally distributed
#
par(mfrow=c(1,2))
qqnorm(comp$mpPAIR) # QQ plots 
qqline(comp$mpPAIR, col = "steelblue", lwd = 2) 
plot(comp$mpPAIR) # histogram of distribution 
#

### --- Non-transformed [CN] value correlations between Meter and Spectrophotometer
# This is the data that is presented in the mansucript because log and log10 transormations 
#    ... did not change the directionality or significance of the results
comp=read.csv("method.comparison3.csv", header=T)
comp2=comp[-c(16),]
head(comp)

#### CUP vs SPEC
corr2=lm(cupPAIR~spec.cup + species,data=comp2)
summary(corr2)
# Residual standard error: 5.621 on 28 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.6397,	Adjusted R-squared:  0.6269 
# F-statistic: 49.72 on 1 and 28 DF,  p-value: 1.138e-07
#

corr3=lm(cupPAIR~spec.cup + species,data=comp2)# with species term added to model
summary(corr3)
# 0.76 (model w/ species term) - 0.63 (w/o species term) = 0.13 additional variance explained by species
# (R-squared = 0.6185 if species not included in model)
### TAKE HOME MESSAGE:
#   The cup method is a good approximation of the classic Lambert CN quantification method. 
#   This is epecially true for some species. 

### P.menspermifolia contribute significantly to this correlation.
# These next few lines explore correlations for data on these species.
MEN <- comp2[which(comp2$species == 'P.menispermifolia'), 
             names(comp2) %in% c('species','unique','cupPAIR','spec.cup','mpPAIR','spec.mp')]
mc=lm(cupPAIR~spec.cup ,data=MEN)
summary(mc) # Adjusted R-squared:  0.7676 
# plot(cupPAIR~spec.cup ,data=MEN)
# abline(lm(cupPAIR~spec.cup ,data=MEN), col="blue")
#

### MP vs SPEC
corr4=lm(mpPAIR~spec.mp,data=comp2)
summary(corr4)
# Residual standard error: 3.94 on 29 degrees of freedom
# Multiple R-squared:  0.7257,	Adjusted R-squared:  0.7163 
# F-statistic: 76.74 on 1 and 29 DF,  p-value: 1.215e-09
#
corr5=lm(mpPAIR~spec.mp + species,data=comp2) # with species term added to model
summary(corr5)
# 0.8311 (model w/ species term) - 0.72 (w/o species term) = 0.11 additional variance explained by species
# (R-squared = 0.72 if species not included in model)
## TAKE HOME MESSAGE:
#The cup method is a good approximation of the classic Lambert CN quantification method. 
#   This is epecially true for some species. 

# P.menspermifolia contribute significantly to this correlation.
MEN <- comp2[which(comp2$species == 'P.menispermifolia'), 
             names(comp2) %in% c('species','unique','cupPAIR','spec.cup','mpPAIR','spec.mp')]
mc2=lm(cupPAIR~spec.cup ,data=MEN)
summary(mc2) # Adjusted R-squared:   
#

#### --- natural LOG-transformed [CN] value correlations 
#### CUP vs SPEC
comp3=read.csv("method.comparisonCUP.csv", header=T)
colnames(comp3)
cupLOG <- log(comp3$cupPAIR) # log transform the data
cupspecLOG <- log(comp3$spec.cup) # log transform the data
# 
corrlog=lm(cupLOG~cupspecLOG)
summary(corrlog)
# Multiple R-squared:  0.5499,	Adjusted R-squared:  0.5349 
# F-statistic: 36.65 on 1 and 30 DF,  p-value: 1.2e-06
#
#### MP vs SPEC
mpLOG <- log(comp2$mpPAIR) # log transform the data
mpspecLOG <- log(comp2$spec.mp) # log transform the data
# stats on log MP data
corrlog2=lm(mpLOG~mpspecLOG,data=comp2)
summary(corrlog2)
# Multiple R-squared:  0.5707,	Adjusted R-squared:  0.5548 
# F-statistic:  35.9 on 1 and 27 DF,  p-value: 2.164e-06
#

### --- log10-transformed [CN] value correlations
comp=read.csv("method.comparison3.csv", header=T)
comp2<-comp[-c(16),]
head(comp2)
colnames(comp2)

#### CUP vs SPEC
cupLOG <- log10(comp3$cupPAIR) # log10 transform the data
cupspecLOG <- log10(comp3$spec.cup) # log10 transform the data
corr10log=lm(cupLOG~cupspecLOG,data=comp2)
summary(corr10log)
# Residual standard error: 0.3841 on 28 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.6997,	Adjusted R-squared:  0.689 
# F-statistic: 65.24 on 1 and 28 DF,  p-value: 8.541e-09

###### MP vs SPEC
mpLOG <- log10(comp2$mpPAIR) # log10 transform the data
mpspecLOG <- log10(comp2$spec.mp) # log10 transform the data
corr10log2=lm(mpLOG~mpspecLOG,data=comp2)
summary(corr10log2)
# Residual standard error: 0.4437 on 29 degrees of freedom
# Multiple R-squared:  0.6167,	Adjusted R-squared:  0.6035 
# F-statistic: 46.66 on 1 and 29 DF,  p-value: 1.678e-07
#


### -------------------------------------------------------------------------------------------
### -------------------------------------------------------------------------------------------
#### ------------------- Figure 7 Method Calibration Experiment - Desiccator vs Cup

dess=read.csv('dessicate.csv', header=T)
dess2=dess[-c(1:2),]
head(dess2)
colnames(dess)

corr6=lm(dessicator~cup,data=dess2)
summary(corr6)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.4329     0.7971   0.543    0.596    
# cup           9.9644     0.7033  14.169 2.78e-09 ***

# Multiple R-squared:  0.9392,	Adjusted R-squared:  0.9345 
# F-statistic: 200.8 on 1 and 13 DF,  p-value: 2.783e-09


###  Calculate ratio using log10 values
corr7=lm(log10dessicator~log10cup,data=dess2)
summary(corr7)
#  Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  1.01647    0.01407   72.22  < 2e-16 ***
#  log10cup     0.95084    0.04268   22.28 9.68e-12 ***

# Residual standard error: 0.05208 on 13 degrees of freedom
# Multiple R-squared:  0.9745,	Adjusted R-squared:  0.9725 
# F-statistic: 496.4 on 1 and 13 DF,  p-value: 9.683e-12



### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  -----------FIGURE XXX - Standard Curve with Amygdalin 
# Here I am creating cyanogenic reactions in the lab with known amounts of the commercially available 
#   cyanogenic glycoside amygdalin, and endogenous beta-glucosidase enzyme isolated from almond seeds
# Aliquots of the reaction mixture are placed in the plastic cup and measured with the HCN meter to 
#   create a standard curve for user reference. 

amy=read.csv('amygdalin.csv', header=T)
colnames(amy)
head(amy)

### subset cup and MP data for easy plotting
cup <- amy[which(amy$chamber == 'cup'), names(amy) %in% c('sample','chamber','ppmTOTAL','ppmAVG','mg','umol')]
MP <- amy[which(amy$chamber == 'MP'), names(amy) %in% c('sample','chamber','ppmTOTAL','ppmAVG','mg','umol')]


### linear regression to calculate linear correlation and calibration equation
# -- cup
cor1<-lm(mg~ppmAVG,data=cup)
summary(cor1) 
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -0.002536   0.237128  -0.011  0.99214   
# ppmAVG       0.030899   0.002525  12.239  0.00117 **
# Adjusted R-squared: 0.9738 
# slope equation to calculate mg HCN: y = 0.031x - 0.0025

# -- MP
cor2<-lm(mg~ppmAVG,data=MP)
summary(cor2) 
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.197696   0.133697  -1.479  0.23576    
# ppmAVG       0.043016   0.001871  22.988  0.00018 ***
# Adjusted R-squared:0.9904 
# slope equation to calculate mg HCN: y = 0.043x - 0.1977


### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
#### ----------- FIGURE 13 - Heliconius doris HCN Induction Figure  
# Goal is to analyze induction of HCN by H.doris caterpillars feeding on P. ambigua leaves

sync=read.csv("Hdoris.feeding.csv", header=T)

head(sync)
colnames(sync)
nrow(sync) #94
sync2<-sync[-c(21:23),]
nrow(sync2) #91
#subset into control and herbivory leaf objects 
levels(sync$treatment)
nofeed <- sync2[which(sync2$treatment == 'no herbivory'), names(sync2) %in% c('date','species','treatment','leaf','HCN')]
feed <- sync2[which(sync2$treatment == 'larvae feeding'), names(sync2) %in% c('date','species','treatment','leaf','HCN')]

### ---- NORMALITY assessment
# visualize
par(mfrow=c(1,2))
hist(nofeed$HCN)
qqnorm(nofeed$HCN)
qqline(nofeed$HCN, col = "steelblue", lwd = 2) # doesn't appear normally distributed
#
par(mfrow=c(1,2))
hist(feed$HCN)
qqnorm(feed$HCN)
qqline(feed$HCN, col = "steelblue", lwd = 2) # doesn't appear normally distributed

# Shapiro-Wilks test for normality
shapiro.test(nofeed$HCN) # W = 0.91029, p-value = 0.002289
shapiro.test(feed$HCN) # W = 0.9463, p-value = 0.03108
# neither normally distributed
#

### ---- Analysis: t-test
# extract HCN data for aalysis
nofeed2<-nofeed[,c(5)] 
feed2<-feed[,c(5)]

var.test(nofeed2,feed2)
# F = 5.5256, num df = 43, denom df = 46, p-value = 6.001e-08
# alternative hypothesis: true ratio of variances is not equal to 1

t.test(nofeed2,feed2,paired=FALSE,var.equal = FALSE) 
# t = 4.6024, df = 57.268, p-value = 2.364e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.1553307 0.3945618

### Analysis: GLM
# feed=glm(HCN~treatment,data=sync2,family="gaussian")
# summary(feed)
#                 Estimate Std.Error  t value  Pr(>|t|)    
# (Intercept)      0.27732    0.04059   6.832   1.00e-09 ***
# treatmentfeed    0.27495    0.05837   4.710   9.07e-06 ***
#


### -------------------------------------------------------------------------------------------
### -------------------------------------------------------------------------------------------
#### ----- Spectrophotometer Comparison of Intra-Leaf Variation - Experimental Design Validation
# Here I am regressing the [CN] values from an individual leaf against one another to assess inter-leaf variation 
#  .... and potential variation inherenet to method. 

# comp=read.csv("method.comparison.csv", header=T)
comp=read.csv("method.comparison3.csv", header=T)
head(comp)
colnames(comp)

### ordinary linear regression
corr6=lm(spec.cup~spec.mp,data=comp)
summary(corr6)
# Residual standard error: 4.532 on 31 degrees of freedom
# Multiple R-squared:  0.5524,	Adjusted R-squared:  0.537 
# F-statistic:  35.8 on 1 and 29 DF,  p-value: 1.669e-06
## Take home message: There is a lot of intra-leaf HCN variation. 
#

### linear mixed effects model 
#library(lme4)

# randomly generate an object of leaf sides (0 and 1) to add to lmer model - to measure random effect of leaf side
# nrow(comp) # 33
# side <- sample(c(0,1), replace=TRUE, size=33)

# model fixed and random effecs
# corr7 <- lmer(spec.cup ~ spec.mp + (1|side) , data=comp,REML=FALSE) #random intercept for effect of leaf side
# summary(corr7)# see model summary
# anova(corr7)# get P values for fixed effects, though this is controversial and can only be done with the lme4 package
# ranef(corr7)# view random effect coefficients



#################################################################################
#################################################################################
#################################################################################
#################################################################################