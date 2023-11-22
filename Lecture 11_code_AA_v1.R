######### NotES


# Clustering
# Many applications - libray, recipe optimization, 
# https://projects.fivethirtyeight.com/polls/



## NP Hard

# approximate solution

########### K Means: 
## min (variance within cluster), min(out of cluster variance) 
### https://en.wikipedia.org/wiki/Millennium_Prize_Problems

## Pick k points
## Randomly alot k colors to each point
## compute centroid for each cluster
## reassign observations to closestn centroid
## repeat until no shuffling point



## McDonal's

mcd = read.csv("C:/Users/aanwar11/Documents/Code Files/Datasets/mcmenu.csv")
mcd2 = read.csv("C:/Users/aanwar11/Documents/Code Files/Datasets/mcmenu_full.csv")

attach(mcd)
mcd_c = mcd[,c(2,3)]
rownames(mcd_c) = mcd$Item

mcd_c

library(ggplot2)
# Plot clusters
plot = ggplot(mcd, aes(y = Sugars, x = Total.Fat))
plot + geom_point()


## plot the clusters

km.2 = kmeans(mcd_c,2)
km.3 = kmeans(mcd_c,3)
km.4 = kmeans(mcd_c,4)
km.5 = kmeans(mcd_c,5)
mcd_c$cluster = as.factor(km.2$cluster)
attach(mcd_c)
plot+geom_point(aes(color = cluster))

# k =3 
mcd_c$cluster = as.factor(km.3$cluster)
attach(mcd_c)
plot+geom_point(aes(color = cluster))

# k=4
mcd_c$cluster = as.factor(km.4$cluster)
attach(mcd_c)
plot+geom_point(aes(color = cluster))

# k=5
mcd_c$cluster = as.factor(km.5$cluster)
attach(mcd_c)
plot+geom_point(aes(color = cluster))

#################

km.2$tot.withinss
km.3$tot.withinss
km.4$tot.withinss
km.5$tot.withinss

clusters = data.frame(rownames(mcd_c), km.3$cluster)
View(clusters)










#############################
# Hierarchical clustering

## Bottom up - 

# closest pair and take avg make it one
# Next closest pair and take avg and make it one
# 


## Create dendogram (hierarchical clustering)
mcd_short = mcd_c[c(1:30),]
rownames(mcd_short) = mcd$Item[c(1:30)]

hc = hclust(dist(mcd_short))
plot(hc, hang = -1, cex = 0.6)

cutree(hc,2) # cut in second
cutree(hc,3) # cut in third height

# https://salvemonos.org/projects/

#### Centroid based
#### Density 
#### Single