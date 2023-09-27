################## NON-LINEAR MODELS ######################
###########################################################
###########################################################


getwd()

setwd("C:/Users/aanwar11/Documents/Code Files/Datasets")

re = read.csv("real_estate_lect5.csv")

library(car) # what is this library
attach(re)

# Y~X => linear regression => multiple linear regression => characteristics (4)

##### why non linear?
##### sometimes linear models are unrealistic
##### non linear models are flexible 


##### We will learn three non linear models
############## 1. POLYNOMIAL MODEL
############## 2.
############## 3. 


######### POLYNOMIAL MODEL
# in a polynomial regression we transform the model to include non-linear terms ( as a polynomial)
# yi = b0 +b1*x
# => yi = b0 +b1xi +b2xi^2 + b3x^3 .............   [Very high flexibility]
## for computer polynomial regression is the same as linear becuase x*x is like interaction


plot(year, price)
plot(bedrooms, price)

reg1 = lm(price~bedrooms + year)
residualPlots(reg1)

#running a polynomial model

reg2 = lm(price~bedrooms+poly(year, 2)) 
reg3 = lm(price~bedrooms+poly(year, 3))
reg4 = lm(price~bedrooms+poly(year, 4))


### intercept
###  variable
### poly(variabl, 2)
### poly(variabl, 3)
### poly(variabl, 4)


# NOTE: lm(y ~ poly(x,1)) will give different result from x~1


reg_a = lm(price~poly(year,1))
reg_b = lm(price~year)
reg_c = lm(price~poly(year,1 ,raw =TRUE)) #without orthogonalization

summary(reg_a)
summary(reg_b)
summary(reg_c)

install.packages("ggplot2")
library(ggplot2)
require(methods)

### Plots Steps

#layer #1: plot environment
plot = ggplot(re, aes(y=price, x=year))
scatter =geom_point()
line_d2 = geom_smooth(method = "lm", formula = y~poly(x,2))
line_d3 = geom_smooth(method = "lm", formula = y~poly(x,3), color = "salmon")
line_d4 = geom_smooth(method = "lm", formula = y~poly(x,4), color = "forest green")


plot+scatter + line_d2
plot+scatter + line_d3
plot+scatter + line_d4
plot+scatter + line_d2 +line_d3 +line_d4


## choosing the optimal polynomial degree
## step 1: choosing models to test (reg1, reg2, reg3, reg4)

## step 2: run anova test
anova(reg1, reg2, reg3, reg4)

#--------------------------------------------------> break


######################################
#### SPLINE REGRESSION ###############
######################################

mt = read.csv("MontrealTemp.csv")
detach(mt)
attach(mt)

plot = ggplot(mt, aes(y=Temperature, x = Day))
scatter = geom_point(color = "grey")
plot + scatter

line_1 = geom_smooth(method="lm", formula = y~x)
line_2 = geom_smooth(method="lm", formula = y~poly(x,2))
line_3 = geom_smooth(method="lm", formula = y~poly(x,3))
line_4 = geom_smooth(method="lm", formula = y~poly(x,4))

plot + scatter +line_1 + line_2 + line_3 + line_4


## splines 
######## simple splines
######## polynomial splines

# linear spline
library(splines)

reg6 = lm(Temperature~bs(Day, knots = c(210, 400, 560), degree = 1))
summary(reg6)

spline_1 = geom_smooth(method ="lm", formula = y~bs(x, knots = c(210,400,560), degree = 1))          
plot + scatter + spline_1


reg6$fitted[200]
reg6$fitted[300]
reg6$fitted[400]
reg6$fitted[500]


reg7 = lm(Temperature~ bs(Day, knots = c(210, 400, 560), degree = 2))
reg8 = lm(Temperature~bs(Day, Knots = c (210, 400, 560), degree = 3))
reg9 = lm(Temperature~bs(Day, Knots = c (210, 400, 560), degree = 4))

spline_1 = geom_smooth(method= "lm", formula = y~bs(x, knots = c(210,400, 560), degree = 1), aes(color = "red"))
spline_2 = geom_smooth(method= "lm", formula = y~bs(x, knots = c(210,400, 560), degree = 2), aes(color = "red"))
spline_3 = geom_smooth(method= "lm", formula = y~bs(x, knots = c(210,400, 560), degree = 3), aes(color = "red"))
spline_4 = geom_smooth(method= "lm", formula = y~bs(x, knots = c(210,400, 560), degree = 4), aes(color = "red"))

spline_plot = plot+scatter+spline_1+ spline_2 + spline_3 + spline_4

spline_plot

k1 = quantile(Day, 0.2)
k2 = quantile(Day, 0.4)
k3 = quantile(Day, 0.6)
k4 = quantile(Day, 0.8)

reg10 =  lm(Temperature~ bs(Day, knots = c(k1, k2, k3, k4), degree = 3))
spline_4 = geom_smooth(method = "lm", formula = y~bs(x, knots =c(k1,k2,k3,k4), degree = 3))
eq_spline = geom_smooth(method = "lm", formula = y~bs(x, knots = c(k1, k2, k3, k4), degree = 3))
plot + scatter + eq_spline + geom_vline(xintercept = c(k1,k2,k3,k4), linetype = "dotted")



### local regression

reg11 = loess(Temperature~Day, span = 0.1)
reg12 = loess(Temperature~Day, span = 0.3)
reg13 = loess(Temperature~Day, span = 0.5)

localplot1 = geom_smooth(method = "loess", span = 0.1, color = "red")
plot + scatter + localplot1

localplot2 = geom_smooth(method = "loess", span = 0.3, color = "red")
plot + scatter + localplot2

localplot2 = geom_smooth(method = "loess", span = 0.6, color = "red")
plot + scatter + localplot2


localplot2 = geom_smooth(method = "loess", span = 0.96, color = "red")
plot + scatter + localplot2


localplot2 = geom_smooth(method = "loess", span = 1.5, color = "red")
plot + scatter + localplot2

localplot2 = geom_smooth(method = "loess", span = 5, color = "red")
plot + scatter + localplot2

localplot2 = geom_smooth(method = "loess", span = 50, color = "red")
plot + scatter + localplot2
