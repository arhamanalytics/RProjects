## Lecture 6
## 231004

getwd()

setwd("C:/Users/aanwar11/Documents/Code Files/Datasets")

re = read.csv("real_estate_lect6.csv")

library(car) # what is this library
attach(re)

install.packages("catools")

require(caTools)
require(splines)
require(methods)
require(ggplot2)


## Underfitting happens when we use a simplistic function to fit 
## overfitting happens when we use splines or a complex function

## two types of errors - training error, test error

### when we overfit, we need to carefull of test error
###


## HOw can we know if our model would perform well with new data => "Resampling"
### train test split

View(re)
attach(re)


plot = ggplot(re, aes(y = price, x = area))
#plot = ggplot(object, aes(y = var1, x = var2))

scatter = geom_point()
#scatter = geom_point()
plot+scatter

########### validation
reg1 = lm(price~area)
reg2 = lm(price~poly(area,2))
reg3 = lm(price~poly(area,10))
summary(reg1)
summary(reg2)
summary(reg3)

################ LINEAR ##################################################

##########################################################################
############# step 1: divide data into two sets ##########################
##########################################################################

sample = sample.split(re$price, SplitRatio = 0.5)

#Assigns rows true false values based on the split ratio
train_set = subset(re, sample ==TRUE)
test_set = subset(re, sample == FALSE)

train_points = geom_point(data = train_set, col ="red")
test_points = geom_point(data = test_set, col = "grey")
plot+train_points+test_points

##########################################################################
############# step 2: fit the model ######################################
##########################################################################

fit = lm(price~area, data = train_set)

#fit_inv = lm(price~area, data = test_set)

##########################################################################
############# step 3 & 4: Calculate the MSE on test data #################
##########################################################################

actual = test_set$price #y
prediction = predict(fit,test_set) # y_hat
squared_error = (actual-prediction)^2
mse = mean(squared_error)
mse


################ Quad ##################################################

##########################################################################
############# step 1: divide data into two sets ##########################
##########################################################################

sample = sample.split(re$price, SplitRatio = 0.5)

#Assigns rows true false values based on the split ratio
train_set = subset(re, sample ==TRUE)
test_set = subset(re, sample == FALSE)

train_points = geom_point(data = train_set, col ="red")
test_points = geom_point(data = test_set, col = "grey")
plot+train_points+test_points

##########################################################################
############# step 2: fit the model ######################################
##########################################################################

fit = lm(price~poly(area,2), data = train_set)

#fit_inv = lm(price~area, data = test_set)

##########################################################################
############# step 3 & 4: Calculate the MSE on test data #################
##########################################################################

actual = test_set$price #y
prediction = predict(fit,test_set) # y_hat
squared_error = (actual-prediction)^2
mse = mean(squared_error)
mse

##########################################################################


################ 10 ##################################################

##########################################################################
############# step 1: divide data into two sets ##########################
##########################################################################

sample = sample.split(re$price, SplitRatio = 0.5)

#Assigns rows true false values based on the split ratio
train_set = subset(re, sample ==TRUE)
test_set = subset(re, sample == FALSE)

train_points = geom_point(data = train_set, col ="red")
test_points = geom_point(data = test_set, col = "grey")
plot+train_points+test_points

##########################################################################
############# step 2: fit the model ######################################
##########################################################################

fit = lm(price~poly(area,10), data = train_set)

#fit_inv = lm(price~area, data = test_set)

##########################################################################
############# step 3 & 4: Calculate the MSE on test data #################
##########################################################################

actual = test_set$price #y
prediction = predict(fit,test_set) # y_hat
squared_error = (actual-prediction)^2
mse = mean(squared_error)
mse

##########################################################################

##########################################################################
#######################       LOOCV   ###################################
##########################################################################

library(boot)
fit = glm(price~area, data = re)
mse = cv.glm(re, fit)$delta[1] # same for everyone
mse

## quad

fit = glm(price~poly(area,2), data = re)
mse = cv.glm(re, fit)$delta[1] # same for everyone
mse

## poly 10

fit = glm(price~poly(area,10), data = re)
mse = cv.glm(re, fit)$delta[1] # same for everyone
mse

######Automate best model search through loops

mse = rep(NA,5) # vector of 5 values with all NA values
for (i in 1:5) {
  fit = glm(price~poly(area,i), data = re)
  mse[i] = cv.glm(re, fit)$delta[1]
}

plot(mse)
lines(mse, col = "red")

##finding minimum mse
which.min(mse)
min(mse)

##########################################################################
#######################     Kfold Cv   ###################################
##########################################################################

# line
fit = glm(price~ area, data = re)
mse = cv.glm(re, fit, K=10)$delta[1]
mse

# quad
fit = glm(price~ poly(area,2), data = re)
mse = cv.glm(re, fit, K=10)$delta[1]
mse

# 10
fit = glm(price~ poly(area,10), data = re)
mse = cv.glm(re, fit, K=10)$delta[1]
mse

## wuth loop

mse = rep(NA,10) # vector of 5 values with all NA values
for (i in 1:10) {
  fit = glm(price~poly(area,i), data = re)
  mse[i] = cv.glm(re, fit, K=10)$delta[1]
}

plot(mse)
lines(mse, col="red")
which.min(mse)
min(mse)

###

mse = rep(NA,5) # vector of 5 values with all NA values
for (i in 1:5) {
  fit = glm(price~poly(area,i), data = re)
  mse[i] = cv.glm(re, fit, K=20)$delta[1]
}

plot(mse)
lines(mse, col="red")
which.min(mse)
min(mse)
