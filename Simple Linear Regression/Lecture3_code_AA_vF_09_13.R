getwd()

setwd("C:/Users/aanwar11/Documents/Code Files/Datasets")

re = read.csv("real_estate_lect3.csv")

attach(re)

### multiple linear regression 
## factors affecting price : year of built, bedrooms, area

View(re)


# run regression
mreg = lm(price~bedrooms+area+year)
summary(mreg)

"""
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 7144.62985 2024.51711   3.529 0.000656 ***
  bedrooms      47.66336   32.04561   1.487 0.140377    
area           0.06621    0.04249   1.558 0.122637    
year          -3.45752    1.00189  -3.451 0.000849 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 320.5 on 91 degrees of freedom
Multiple R-squared:  0.3239,	Adjusted R-squared:  0.3016 
F-statistic: 14.53 on 3 and 91 DF,  p-value: 8.22e-08
"""

#### why not running separate simple linear regressions?

# Reason #1: we cannot make joint predictions

# e.g.

sreg1 = lm(price~bedrooms)
sreg2 = lm(price~area)
sreg3 = lm(price~year)

# summary
summary(sreg1)
summary(sreg2)
summary(sreg3)

#predictions of sreg1. sreg2. and sreg3

b0 = coef(sreg1)[1]
b1 = coef(sreg1)[2]
b0+b1*7


b0 = coef(sreg2)[1]
b1 = coef(sreg2)[2]
b0+b1*2500


b0 = coef(sreg3)[1]
b1 = coef(sreg3)[2]
b0+b1*2008


#joint prediction using mreg

b0 = coef(mreg)[1]
b1 = coef(mreg)[2]
b2 = coef(mreg)[3]
b3 = coef(mreg)[4]


#predictions for 7-bedroom home with area = 2500, year = 2008

b0 + b1*7 + b2*2500 + b3*2008



########################################################
# Categorical data
# Categorical data is of two types - binary, or many values


#step 1: Declare variables as dummy/factor variables
re$property_type = as.factor(re$property_type)
attach(re) #attaching is like taking a ss, and sicnce we changed the dataset we meed tp reattach it

#step 2: Explore the categories
levels(property_type) #shows you the categoreis in the variable
table(property_type) #shows you number of obs in each categpry


#step 3: run regression with dummy
mreg2 = lm(price~bedrooms+property_type)
summary(mreg2)

#step 4: Plot regeression lines
plot(bedrooms, price, col=ifelse(property_type=="house","red","blue"))

b0 = coef(mreg2)[1]
b1 = coef(mreg2)[2]
b2 = coef(mreg2)[3]

abline(b0+b2, b1, col='red') # intercept = b0+b2, slope =b1
abline(b0,b1, col = 'green') # intercept = b0, slope = b1



### practice for  other categorical variables
View(re)
re$neighbourhood = as.factor(re$neighbourhood)
attach(re)
levels(neighbourhood)
table(neighbourhood)

mreg3 = lm(price~bedrooms+neighbourhood)
summary(mreg3)


#Note - to change excluded category, we can use 'relevel'
re$neighbourhood = relevel(re$neighbourhood, ref ="Lasalle")
attach(re)
mreg4 = lm(price~bedrooms+neighbourhood+property_type)
levels(neighbourhood)
table(neighbourhood)



###Interaction terms:

mreg5 = lm(price~bedrooms + property_type + bedrooms*property_type)
summary(mreg5)

b0 = coef(mreg5)[1]
b1 = coef(mreg5)[2]
b2 = coef(mreg5)[3]
b3 = coef(mreg5)[4]

plot(bedrooms, price, col = ifelse(property_type = 'house', 'red', 'blue'))
abline(b0+b2, b1+b3, col = 'red')
abline(b0,b1, col = 'blue') 
