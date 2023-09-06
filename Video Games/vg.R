#lab 1

# read data
vg = read.csv("C:/Users/aanwar11/Documents/Code Files/Video Games/video_games_fall_2023.csv")

# attach data
attach(vg)


names(vg)
View(vg)



### Question 1

##1.i

par(mfrow = c (1,2))

## 1.i.a.
summary(score)

## 1.i.b.
boxplot(score, col = "blue")

## 1.i.c.
hist(score, breaks = 20, col = "blue")


##1.ii

par(mfrow =c (1,2))

## 1.ii.d
summary(sales_global)

##1.ii.e
boxplot(score, col = "green")

##1.ii.f 
hist(score, col = "green", breaks = 100)



##1.iii

par(mfrow =c (1,2))

## 1.iii.g
summary(release_year)

##1.iii.h
boxplot(release_year, col = "orange")

##1.iii.i 
hist(release_year, col = "orange", breaks = 20)

##1.iv

par(mfrow =c (1,2))

## 1.iv.j
summary(count_critic)

##1.v.k
boxplot(count_critic, col = "purple")

##1.vi.l 
hist(count_critic, col = "purple", breaks = 10)



### 1.l

par(mfrow =c (1,3))

plot(sales_global ,score, col = factor(genre))
plot(release_year ,score, col = factor(genre))
plot(count_critic ,score, col = factor(genre))


