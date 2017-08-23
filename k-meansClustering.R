

setwd("/Users/sjain/Downloads")
cluster <- read.csv("cluster.csv",sep=",", header=TRUE, stringsAsFactors = FALSE ) 

head(cluster)

#cluster.stand <- scale(cluster[-1])
interests_z <- as.data.frame(lapply(cluster, scale))
k.means.fit <- kmeans(interests_z, 5)
attributes(k.means.fit)
# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster

# Cluster size:
k.means.fit$size

wssplot <- function(data, nc=36, seed=30000){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(interests_z, nc=6) 

install.packages("cluster")
library(cluster)
clusplot(interests_z, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

pie(colSums(cluster[k.means.fit$cluster==2,]),cex=0.5)

pie(colSums(cluster[k.means.fit$cluster==3,]),cex=0.5)

pie(colSums(cluster[k.means.fit$cluster==4,]),cex=0.5)

pie(colSums(cluster[k.means.fit$cluster==5,]),cex=0.5)

install.packages("fpc")
library(fpc) 
plotcluster(interests_z, k.means.fit$cluster)


