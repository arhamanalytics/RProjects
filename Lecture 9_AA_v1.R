## Regression Trees - Lecture 9

## e.g. 1
## Grow a tree
            # step 1: divide the predictor space into j regions
            # step 2: For every obs in Region R, we find the mean response
                          # minimize RSS, which is summation of difference 
                          # in point and region avg combinations
                          # " Recursive binary splitting "


                    ############### Recursive Binary Splitting ##############
                    ### Step 1 :  choose complexity paramere CP [0,1] ~ higher is less complexity
                    ### Step 2 : find a new branch, determine best branching criterion by looking at predictors and values, find RSS
    

install.packages("bannerCommenter")
library(bannerCommenter)
banner("Section 1:", "libraries", emph = TRUE)










############################################################################
############################################################################
###                                                                      ###
###                              SECTION 1:                              ###
###                              LIBRARIES                               ###
###                                                                      ###
############################################################################
############################################################################                 
                    



install.packages("tree")
install.packages("rpart.plot")
library(tree)
library(rpart)
library(rpart.plot)

data_IMDB = read.csv("C:/Users/aanwar11/Documents/Code Files/Datasets/data_IMDB.csv")

attach(data_IMDB)


banner("Section 2:", "First Tree", emph = TRUE)





############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                              FIRST TREE                              ###
###                                                                      ###
############################################################################
############################################################################

### First Tree

mytree=rpart(imdb_score~duration+title_year,control=rpart.control(cp=0.01))
rpart.plot(mytree)
summary(mytree)



### finding the best cp 

## step 1: Create an overfitted tree (very low cp)
myoverfittedtree = rpart(imdb_score~duration+title_year,control = rpart.control(cp=0.0001))
rpart.plot(myoverfittedtree)

myunderfittedtree = rpart(imdb_score~duration+title_year,control = rpart.control(cp=0.0002))
rpart.plot(myunderfittedtree)

## step 2: Plot the different cp values
printcp(myoverfittedtree)
plotcp(myoverfittedtree)


## step 3:  Find the optimal cp (opt_cp)

opt_cp = myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]), "CP"]
opt_cp


## Step 4: plot the best tree (i.e., )

bestotree = rpart(imdb_score~duration+title_year,control = rpart.control(cp=0.001543018))
rpart.plot(bestotree)

banner("Section 3:", " Why trees", emph = TRUE)






############################################################################
############################################################################
###                                                                      ###
###                              SECTION 3:                              ###
###                               WHY TREES                              ###
###                                                                      ###
############################################################################
############################################################################


# more readable
# can deal really well with categorical predictors
# they suck at prediction, have not as good an accuracy as linear regression
# variance problem, they change a lot ; they were abandoned in 60s 


## bagging algorithm: taking bootstrap samples from dataset and create diff trees
## if i take an average prediction of the bootstrapped trees, "back forrest" 

## wisdom of the crowd

banner("Section 4:", " Random Forrest", emph = TRUE)







###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 4:                             ###
###                            RANDOM FORREST                           ###
###                                                                     ###
###########################################################################
###########################################################################

## difference between random forrest and backforrest
## in random forrest we randomly select 2 predictors -< diversify predictors 
##

#install.packages("randomForest")
library(randomForest)


## create forrest

myforest = randomForest(imdb_score~ budget + duration + title_year 
                        + movie_facebook_likes + actor_1_facebook_likes
                        + facenumber_in_poster, ntrees =500, data = data_IMDB
                        , importance = TRUE, na.action = na.omit)

myforest


## make prediction

predict(myforest, data.frame(movie_facebook_likes = 1000, budget = 1000000, 
                             duration = 90, title_year = 2017,
                             actor_1_facebook_likes = 2500, 
                             facenumber_in_poster =3))

## importance plot - % in MSE, is max for facebook likes

importance(myforest)
varImpPlot(myforest)


###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 5:                             ###
###                           Cross Validation                          ###
###                                                                     ###
###########################################################################
###########################################################################



# to see out of bag performance of our model, i.e., test our 
myforest = randomForest(imdb_score~ budget + duration + title_year 
                        + movie_facebook_likes + actor_1_facebook_likes
                        + facenumber_in_poster, ntrees =500, data = data_IMDB
                        , importance = TRUE, na.action = na.omit, do.trace = 50)



###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 6:                             ###
###                           Classification Trees                      ###
###                                                                     ###
###########################################################################
###########################################################################


## classification forrest

myclassifiedtree=rpart(content_rating~duration+title_year+facenumber_in_poster, cp=0.07,
                        na.action=na.omit)
rpart.plot(myclassifiedtree)





###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 7:                             ###
###                           boosting Trees                            ###
###                                                                     ###
###########################################################################
###########################################################################

# Trees learning from each other 
# individually weak learners but when put together, i.e., 
# an 'ensemble of weak learners' do really well

### generalized boosted models (GBM)

install.packages("gbm")
library(gbm)
set.seed(1) ## get same results
boosted=gbm(imdb_score~budget+duration+title_year+movie_facebook_likes
            +actor_1_facebook_likes+facenumber_in_poster,data=data_IMDB,
            distribution="gaussian",n.trees=10000, interaction.depth=4) 

summary(boosted)



