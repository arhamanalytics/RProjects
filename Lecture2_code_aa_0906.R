# importing dataset
real_estate = read.csv("C:/Users/aanwar11/Downloads/classfiles/Lecture 2/real_estate.csv")

#this tells R r is working only with this dataset
attach(real_estate)

#two basic functions
names(real_estate)  ##Shows names of variables
View(real_estate)   ##shows a spreadsheet view of the data

# Simple linear regression - foundation of ML
# step 1: obtain data
# step 2: visualization  - a. Visualize variables , b. Visualize relationship


##################################################################
####################Visualizing variable##########################
##################################################################

## Visualizing variables independently
# summary statistics
summary(price)
summary(bedrooms)
# summary(real_estate$price)  
# remember that the summary stats relate to a business problem always

## boxplots 
boxplot(price)
boxplot(bedrooms, col = "violet")


## histograms
hist(price)
hist(bedrooms)


## scatterplot
plot(bedrooms, price)


# line that best fits the curve

## training data
# n vectors of (xi, yi) from i = 1 to n
# where yi = price of house i
# and xi = house index
### Training data --> black box --> b^0 and b^1 [ b hat, not b to the power]




##################################################################
########################Training Model############################
##################################################################


# syntax: lm(y-x)
lm.fit = lm(price~bedrooms)
lm.fit


# price = intersect + slope x number of bedrooms

abline(lm.fit, col="sky blue")



## Making predictions
b0 = coef(lm.fit)[1]
b1 = coef(lm.fit)[2]

b0
b1

b0 + b1*2 #prediction for a 2 bedroom
b0 + b1*3 #prediction for a 3 bedroom
b0 + b1*4 #prediction for a 4 bedroom
b0 + b1*5 #prediction for a 5 bedroom
b0 + b1*6 #prediction for a 6 bedroom


# how close our guess is from line

# when standard error is large, the data points are far from the line

summary(lm.fit)


### Null hypothesis, b1 =0
### Alternative hypothesis. b1 !=0
### t test- statistic,   t =  b1 / SEb1 = estimate/Standard Error
### p value (>|t|)

### a ** b 
### higher the stars more
### confidence interval ~ w




# confidence interval


confint(lm.fit, 'bedrooms', level = 0.9) #confidence interval 90%
confint( lm.fit, bedrooms, level = 0.95) #confidence interval 95%
confint( lm.fit, bedrooms, level = 0.99) #confidence interval 99%
# if you build the extra bedrooom, the estimate increase would



#visualizing confidence intervals

# install.packages("visreg") #only need to run once
# package to create visualizations
require(visreg)
visreg(lm.fit, alpha = 0.05) ##95% C.I. (alpha = 1 - 0.95)



par(mfrow = c (2,3))
#60% confidence
visreg(lm.fit, alpha = 1- 0.6)
#70% confidence
visreg(lm.fit, alpha = 1- 0.7)
#80% confidence 
visreg(lm.fit, alpha = 1- 0.8)
#90% 
visreg(lm.fit, alpha = 1- 0.9)
#95%
visreg(lm.fit, alpha = 1- 0.95)
#97.5%
visreg(lm.fit, alpha = 1- 0.975)

# reset plot environment

par(mfrow = c (1,1))



#  R squared goes from 0 to 1 and can be seen in summary
summary(lm.fit)

#multiple r-squared, adjusted r-squared