
x = read.csv("diabetes_data_preprocessed_dropped_duplicates_1.csv", header = TRUE, sep = ",")

#y=x[,c(4:16,42)]
y=x[,c(9:11,42)]
library(cluster)

# Determine number of clusters with kmeans
wss <- (nrow(y)-1)*sum(apply(y,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(y,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Normalization
m<- apply(y,2,mean)
s<- apply(y,2,sd)

y<-scale(y,m,s)
#d <- dist(y)


km <- kmeans(y, 3) 
km

#sil <- silhouette(km$cl, d)
#windows()
#plot(sil)


# K-Means Cluster Analysis
agg = aggregate(y,by=list(km$cluster),FUN=mean) 
# km$cluster is the cluster index of each element
# aggregate -> Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form
y <- data.frame(y, km$cluster) # append cluster assignment
print(agg) # shows center of each cluster
clusplot(y, km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, cex = 0.7)


#y=x[,c(4:8,42)]  5cluster       not so cool clusters
#y=x[,c(5:8,42)] 3cluster        so cool clusters
#y=x[,c(4:5,7:8,42)] 3 cluster   in the middle coolness
#y=x[,c(5,7:8,42)]  3 cluster    so cool clusters
#y=x[,c(9:12,42)] 3 cluster     not so cool clusters
#y=x[,c(9:11,42)] 4 cluster     so cool