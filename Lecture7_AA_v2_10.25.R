################## Lecture 7         ######################
###########################################################
###########################################################

getwd()

setwd("C:/Users/aanwar11/Documents/Code Files/Datasets")

sl = read.csv("shot_logs - v2.csv")

library(car)
attach()

require(caTools)
require(splines)
require(methods)
require(ggplot2)

# when Y is category
# Menu ->  logistic regression, linear discriminant analysis, quadratic discriminant analysis, classification trees


##################Linear Probability Model#################
###########################################################
###########################################################

# basketball: score or no score
# how close should you be 
#
#
#



# how likely is it for an nba player to score based on distance from net

## step 1 : we let dependent variable be a dummy variable called Score [50* 94]

attach(sl)

View(sl)

names(sl)

# Dummify shot result 

SHOT_RESULT = as.factor(SHOT_RESULT)
table(SHOT_RESULT)

### CReate {0.1} vaiable called score out of SHOT_RESULT

sl$score = ifelse(SHOT_RESULT == "made",1,0)
attach(sl)
table(score)

library(ggplot2)
require(methods)
plot = ggplot(sl, aes(y= score, x =SHOT_DIST))
scatter = geom_point()
plot + scatter


### Linear Probability Model (LPM)
lpm = lm(score~SHOT_DIST)
summary(lpm)

# probability of scoring at dist = 0 is 60%, whereas for every foot moved away
# from net, probability of scoring decreases by 1.07%

# problems with the model -> 
        ## No bounds [larger than 60 dist we get negative values]
        ## Probabilities in reality are not linear

line  = geom_smooth(method = 'lm' , formula = y~x) # plot probabilities
plot+scatter+line

# in reality the model is naturally s shped so wi ts better to use logistic regression


logit = glm(score~ SHOT_DIST, family = "binomial")
summary(logit)

#obtaining coefficients 

b0 = coef(logit)[1]
b1 = coef(logit)[2]

# prediction for distance = 5

dist = 5
m = exp(b0 + b1*dist)
m/(1+m)


# prdiction fro disacne  =20
dist = 20
m = exp(b0+ b1*dist)
m/(1+m)

# prdiction fro disacne  =20
dist = 40
m = exp(b0+ b1*dist)
m/(1+m)


### finding predicted values more easily

values = data.frame(SHOT_DIST = c(5,20,40,100,120))
predict(logit, values, type = "response")

line = geom_smooth(method = "glm" , formula = y~x,
                  method.args = list(family = binomial))

plot + scatter + line


## multiple logistics regression

mlogit = glm(score~ PERIOD+LOCATION+DRIBBLES+SHOT_NUMBER+SHOT_DIST+CLOSE_DEF_DIST, family = "binomial")
summary(mlogit)

## making predictions

Prob_1 = data.frame(PERIOD = 3, LOCATION = "H", DRIBBLES = 5, SHOT_NUMBER = 3, SHOT_DIST = 20, CLOSE_DEF_DIST =5)
predict(mlogit, Prob_1, type = "response")

Prob_1 = data.frame(PERIOD = 2, LOCATION = "A", DRIBBLES = 2, SHOT_NUMBER = 1, SHOT_DIST = 5, CLOSE_DEF_DIST =1)
predict(mlogit, Prob_1, type = "response")


## R-Sqaured

# new package called rms

install.packages("rms")
require(rms)
mlogit2 =  lrm(score~ PERIOD+LOCATION+DRIBBLES+SHOT_NUMBER+SHOT_DIST+CLOSE_DEF_DIST, data = sl)
mlogit2











