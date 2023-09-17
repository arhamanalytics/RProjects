#lab 1

setwd("/Users/arham/RProjects/Videos Games")

# read data
vg = read.csv("video_games_fall_2023 (1).csv")

# attach data
attach(vg)

### 2 Simple Linear Regression

## 2A

par(mfrow =c(1,1))
plot (sales_global, score)

#Construct regression b0 + b1* = 
lm.fit2 = lm(score~sales_global, data = vg)


n0 = coef(lm.fit2)[1]
n0
n1 = coef(lm.fit2)[2]
n1

summary(lm.fit2)
confint(lm.fit2, level = 0.95)

require(visreg)
visreg(lm.fit, alpha = 0.05) ##95% C.I. (alpha = 1 - 0.95)


#2b

## 2A

par(mfrow =c(1,1))
plot (release_year, score)

#Construct regression b0 + b1* = 
lm.fit = lm(score~release_year)
b0 = coef(lm.fit)[1]
b0
b1 = coef(lm.fit)[2]
b1
summary(lm.fit)
confint(lm.fit, level = 0.95)

require(visreg)
visreg(lm.fit, alpha = 0.05) ##95% C.I. (alpha = 1 - 0.95)

#2c


par(mfrow =c(1,1))
plot (count_critic, score)

#Construct regression b0 + b1* = 
lm.fit = lm(score~count_critic)
b0 = coef(lm.fit)
b0
b1 = coef(lm.fit)[2]
b1
summary(lm.fit)
confint(lm.fit, level = 0.95)

require(visreg)
visreg(lm.fit, alpha = 0.05) ##95% C.I. (alpha = 1 - 0.95)



### 3

## 3A
ans3a= coef(lm.fit)[1] + coef(lm.fit)[2]*0.75

## 3B
ans3b = coef(lm.fit2)[1] + coef(lm.fit2)[2]*2009

## 3c
ans3c = coef(lm.fit3)[1] + coef(lm.fit3)[2]*80


## 4a Reasons:
# we can't make joing predictions
# Individual regressions give inflated/deflated predictions due to the fact that the variable is considered to be the only factor

## 4b 

mreg_q4 = lm(score~sales_global+release_year+count_critic)

summary(mreg_q4)


## 4c

coef(mreg_q4)[1] + coef(mreg_q4)[2]*0.75 + coef(mreg_q4)[3]*2009 + coef(mreg_q4)[4]*80





#5a

vg$Nintendo <- ifelse(vg$publisher == "Nintendo", 1, 0)
attach(vg)
mreg_q5 = lm(score~release_year+Nintendo)

coef(mreg_q5)[1]
coef(mreg_q5)[2]
coef(mreg_q5)[3]



#5b

#5c
b0 = coef(mreg_q5)[1]
b1 = coef(mreg_q5)[2]
b2 =  coef(mreg_q5)[3]

plot(release_year, score, col = ifelse(Nintendo == 1, 'green', 'blue'), pch = 22)
abline(b0, b1, col = 'blue',lwd = 2, lty = 2)
abline(b0+b2, b1, col = 'green', lwd = 2, lty = 2)

legend("bottomleft", 
       legend = c("Nintendo", "Others"), 
       col = c("green", "blue"), 
       lty = 2, 
       title = "Color Legend")

b0
b1
b2

#6

table(genre)
length(unique(title))

#exploring other options
unique_games <- unique(vg[, c('title', 'genre')])
unique_counts <- table(unique_games$genre)
print(unique_counts)



vg$genre <- as.factor(vg$genre)
vg$genre <- relevel(vg$genre, ref = "Racing")
attach(vg)

mreg_q6  = lm(score~genre)
summary(mreg_q6)
coef(mreg_q6)

#q7
vg$Strategy = ifelse(vg$genre == "Strategy", 1, 0)
attach(vg)
mreg_q7 = lm(score~Nintendo+Strategy+Nintendo*Strategy)
summary(mreg_q7)

b0 = coef(mreg_q7)[1]
b1 = coef(mreg_q7)[2]
b2 =  coef(mreg_q7)[3]
b3 =  coef(mreg_q7)[4]

#7c
mreg_q7a = lm(score~release_year+Nintendo+Nintendo*release_year)
summary(mreg_q7a)

b0 = coef(mreg_q7a)[1]
b1 = coef(mreg_q7a)[2]
b2 = coef(mreg_q7a)[3]
b3 = coef(mreg_q7a)[4]

plot(release_year, score, col = ifelse(Nintendo == 1, 'green', 'blue'), pch = 25)
abline(b0+b2, b1+b3, col = 'green',lwd = 2, lty = 2)
abline(b0, b1, col = 'blue', lwd = 2, lty = 2)

legend("bottomleft", 
       legend = c("Nintendo", "Non-Nintendo"), 
       col = c("green", "blue"), 
       lty = 2, 
       title = "Color Legend")

b1+b3

