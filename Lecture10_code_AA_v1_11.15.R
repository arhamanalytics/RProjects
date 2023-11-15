library(bannerCommenter)
banner("Section 1:", "PCA", emph = TRUE)

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 1:                              ###
###                                  PCA                                 ###
###                                                                      ###
############################################################################
############################################################################


## Unspervised Learning - 

# Unsupervised learning - identify and make patterns in given input without initial labels
# Challenge 
        # New field of study ~ not as well understood
        # No predefined task


# --------------------------------------------------------------------------

## For class - 

# 1. Principal Component  Analysis
# 2. Clustering

# --------------------------------------------------------------------------


## PCA 

          # visually scatter plots, for p variables -> p*(p-1)/2 scatterplots
          # reduce dimensions ~ as in map

View(spotify)
attach(spotify)


# seperate numerical and categorical
spotify_labels = spotify[,c(1,2)]
spotify_vars = spotify[, c(3:11)]

install.packages("GGally")

library(ggplot2)
library(GGally)

ggpairs(spotify_vars)


## there has got to be a better way

# --------------------------------------------------------------------------
# Face e.g. - focus on impt features - features with high variance
# why? so we dont spend all of our energy to make conclusions




# --------------------------------------------------------------------------
# n observation p variables 
      # z1 =  phi11 X1 + phi21 X2 + .......... + Phip1 Xp]

# first  # maximize variance -> Optimization problem
# second # maximize variance -> Corr {z2,Z1} = 0 ; 


# --------------------------------------------------------------------------

## principal component analysis 

pca = prcomp(spotify_vars, scale = TRUE)
pca


### Plotting the first and second principal components
install.packages("ggfortify")
library(ggfortify)
autoplot(pca, data = spotify_vars, loadings = TRUE, loadings.label = TRUE)


autoplot(pca, data = spotify_vars, 
         col = ifelse(spotify_labels$artist == "Drake", "skyblue" , "transparent"),
         loadings = TRUE, loadings.label = TRUE)

autoplot(pca, data = spotify_vars, 
         col = ifelse(spotify_labels$artist == "Arcade Fire", "red" , "transparent"),
         loadings = TRUE, loadings.label = TRUE)


autoplot(pca, data = spotify_vars, 
         col = ifelse(spotify_labels$song_title == "Beat It", "brown" , "transparent"),
         loadings = TRUE, loadings.label = TRUE)


## why is


# ---------------------http://127.0.0.1:22713/graphics/plot_zoom_png?width=1308&height=861-----------------------------------------------------


## pc1 -> 
## PC2 -> 
## PC3 ->

# HOW MANY pc should we have? 
# We would like to have a plot of reliability vs #PC

####Percentage-of-variance-explained (PVE) plot

pve = (pca$sdev^2)/sum(pca$sdev^2)
par(mfrow = c(1,2))
plot(pve, ylim = c(0,1))
plot(cumsum(pve), ylm = c(0,1))

## Who uses PCA?
### PCA has many applications - image processing, voic recognition,
### file compression, portfolio management, genetics

IMDB = read.csv("C:/Users/aanwar11/Documents/Code Files/Datasets/IMDB_PCA.csv")
View(IMDB)
attach(IMDB)
IMDB_labels = IMDB[,c(1)]
IMDB_vars = IMDB[,c(2,13)]
pca_IMDB = prcomp(na.omit(IMDB_vars), scale = TRUE)
autoplot(pca_IMDB, data = na.omit(IMDB_vars), loadings = TRUE,col = "grey", loadings.label = TRUE)





