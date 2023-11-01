getwd()

setwd("C:/Users/aanwar11/Documents/Code Files/Datasets")

charity = read.csv("C:/Users/aanwar11/Documents/Code Files/Datasets/charity.csv")

charity$cause = as.factor(charity$cause)
attach(charity)

View(charity)

library(ggplot2)
ggplot(charity, aes(x=cause, y = age)) + geom_boxplot()


### estimated pprior probabilities

table(cause)

pi_deforestation = 31/85
pi_ocean = 26/85
pi_animal = 28/85

### estimating probability density function or posterior probability
## can use normal distribution as assumption and estimate any point using mean and sd

## avg age for each cause 

## ca;culating the means
hists = ggplot(charity, aes(x=age))+geom_histogram(bins = 50)+ facet_grid(cause)
hists

# similar spread
# similar distribution

mean(age[cause == "animal rights"])
mean(age[cause == "deforestation"])
mean(age[cause == "ocean cleanup"])
sd(age)

### install packages for linear discriminant analysis
install.packages("MASS")
install.packages("klaR")

library(MASS)
library(klaR)

mylda = lda(cause~age)
mylda


## Grop means and prior probabilities from 



mylda2 = lda(cause~age+income)
mylda2
## predictions
predict(mylda2,data.frame(age = 30, income = 40))
predict(mylda2, data.frame(age = 35, income = 65))
predict(mylda2, data.frame(age = 50, income = 100))

predict(mylda2)


# partition matrix

partimat(cause~income+age, method ="lda", image.colors = c("light grey", "light green", "white"))



# lines are straight because sd for all classes was assumed to be the same but 
# it may or may not be same

## Quadratic Discriminant Analysis

myqda = qda(cause ~ age + income)
predict(myqda, data.frame(age = 25, income = 40))
predict(myqda, data.frame(age = 36, income = 65))
predict(myqda, data.frame(age = 45, income = 100))

partimat(cause ~ income +age, method = "qda", image.colors = c("light grey", "light green", "white"))


partimat(cause ~ income +age, method = "naiveBayes", image.colors = c("light grey", "light green", "white"))
         # extremely easy to overfit

