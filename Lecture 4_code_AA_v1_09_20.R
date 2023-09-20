# Lecture 4, Sept 20

getwd()

setwd("C:/Users/aanwar11/Documents/Code Files/Datasets")

re = read.csv("real_estate_lect4.csv")

attach(re)

View(re)

# install.packages("car")

## Objectives of lecture

# increase predicting power
# how to select predictors that play well as a team

## section 2 > 
#### is there a relationship between x1, x2, x3, x4 ...
#### to do this we set up null hypothesis for each bi for bi = 0 
#### we can also do this for intercept


## Analayzing significance

reg1 = lm(price~bedrooms+area+property_type)
summary(reg1)

##adjusted r square

#let's start with a simple model, with bedrooms as only predictors

reg2 = lm(price~bedrooms)
summary(reg2)
#r2 =21.22%, adj r2 = 20.37%

#let's add an extra variable to reg2 
reg3 = lm(price~bedrooms+area)
summary(reg3)
#r2 = 23.54% , dj r2 = 21.88%

yolo_reg = lm(price~bedrooms+ID)
summary(yolo_reg)
#r2 = 21.43%, adj r2 = 19.72%

#################################################
### The four main issues in linear regression ###
#################################################

## Issue 1: Non Linearity of predictors
library(car)

reg4 =lm(price~area+year+bathrooms+bedrooms)
residualPlot(reg4)
residualPlots(reg4)

# not to trust eyes
# p values should not have a star, if the value is close to 0.1 it is danger zone

## variable year is not linear; Tukey test shows that entire model is not linear

## solution: let's try to remove year

reg5 = lm(price~bathrooms+bedrooms+area)
residualPlots(reg5)
summary(reg5)

### when heteroskedasticiy is present
# coefficients don't change
# t-statistics are artificially high or low
# p values are artifically high or low


### Issue 2: Heteroskedasticity
reg6 = lm(price~area+year+bedrooms)

#step 1: Detect heteroskedasticity visually (funnel test)
residualPlot(reg6, quadratic = False) #quadratic to not see the blue line

#step2: detect heteroskedasticity numerically (non constant variance test)
ncvTest(reg6)


#step3: Correct for heteroskedasticity
#install.packages("lmtest")
#install.packages("plm")

require(lmtest)
require(plm)

summary(reg6)
coeftest(reg6, vcov = vcovHC(reg6,type="HC1"))


### Issue 3: Outliers #######

attach(re)
## Step 1: Identify outliers visually (studentized residual plot)
reg7 = lm(price~area+year+bedrooms)
qqPlot(reg7, envelope = list(style ="none"))

## Step2: bonferroni outlier test

outlierTest(reg7)

## Step 3: Rerun regression without outliers

re2 = real_estate[-c(29,74),] #removes row 29 and 74
attach(re2)
reg8 = lm(price~area+year+bedrooms, data = re)
summary(reg8)

# if outliers don't change r2, we dont remove them

### Issue 4: Collinnearity (aka double counting)

# install.packages("psych")
require(psych)

##Step 1: Detecting collinear variables through correlation matrix

quantvars = re[, c(3,6,7,8,9,10,12)]
corr_matr = cor(quantvars)
round(corr_matr,2)

## area and area with garden have high multicollinearity 

## Step 2: Variance Inflation Factor (VIF) test
reg9 =lm(price ~ bedrooms + bathrooms +area+ area_with_garden +Levels+ year)
vif(reg9)

reg10 =lm(price ~ bedrooms + bathrooms +area+Levels+ year)
vif(reg10)

summary(reg9)
summary(reg10)
