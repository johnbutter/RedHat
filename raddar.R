# Clustering DSIs
# Read in the transposed and normalized dataset
clusters <- read.csv(file="clustertransponsednormalized.csv", sep=',', header=T, row.names=1)
# Begin EDA
str(clusters) # variables by their first 10 observations 
# Normalization
#clusters.p <- as.matrix(clusters) # convert to matrix
#z <- clusters[,-c(1,1)] #take out the first column
z <- as.matrix(clusters) # convert to matrix
m <- apply(z,2,mean) # find mean for all the variables for columns
s <- apply(z,2,sd) # find the standard deviations for columns
z <- scale(z,m,s) # calculate normalized dataset based on the mean and sd
# Caluclating Euclidean distance
distance <- dist(z) # look for columns and rows with small values indicating high correlation
# Caluclating Mahalanobis distance
sq.distance <-mahalanobis(x = z, center = colMeans(z), cov=cov(z), tol=1e-53) #tol provides a fix for singular data.
mahalanobis.distance <- sqrt(sq.distance)
plot(sqrt(sq.distance, labels=DSI$clusters))
print(distance, digits=3) # compact view of values
#Cluster Dendrogram with Complete Linkage
hc.c <-hclust(distance) 
#plot(hc.c, labels=datatset$variable) # plot with labels
plot(hc.c, hang= -1) # plot dengrogram with all labels ending on the same line
# Cluster Dendogram with Average linkage
hc.a <- hclust(distance, method = "average")
plot(hc.a, hang= -1) # dengrogram plot with all labels ending on the same line

#Cluster Membership
member.c <- cutree(hc.c,3) # defining membership into 3 clusters using complete linkage
member.a <- cutree(hc.a,3) # defining membership into 3 clusters using average linkage
table(member.c, member.a) # table of how clustering is done using complete vs. average linkage

# cluster means
aggregate(z, list(member.c), mean) #calculates the averge values for each cluster
aggregate(dataset, list(member.c),mean) #calculates the average values for each cluster using the original units for ease of interpretation
# Silhouette Plot
library(cluster) #bring the cluster package into the workspace
plot(silhouette(cutree(hc.c, 3), distance))  # silhouette plot visualizes clusters and rates high and low membership of each variable into cluster 
# uses Euclidean distance and the cutree function

# kmeans clustering
DSICluster <-kmeans(z,3) # 3 clusters using kmeans
DSICluster #defininig cluster inclusion 
DSICluster$cluster # cluster membership
DSICluster$centers # average values in each cluster
# look at original scatter plot of two variables and define by clsuters.
# plot(variable - variable, dataset, col-kc$cluster) # scatter plot of two variables using original values and color by the kmeans clusters

# Cluster terms together
# read-in updated RTV with Term Frequencies
TermClusters <- read.csv(file="clusternormalized.csv", sep=',', header=T, row.names=1)
#standardize this dataset so that nothing is over valued
zt <- as.matrix(TermClusters) # convert to matrix
# mt <- apply(zt,2,mean) # find mean for all the variables for columns
# st <- apply(zt,2,sd) # find the standard deviations for columns
# zt <- scale(zt,mt,s) # calculate normalized dataset based on the mean and sd
zt <- scale(zt) 
# Caluclating Euclidean distance
distancet <- dist(zt) # look for columns and rows with small values indicating high correlation
print(distancet, digits=3) # compact view of values
# mahalanobis distance
sq.distancet <-mahalanobis(x = zt, center = colMeans(zt), cov=cov(zt), tol=1e-53)) 
mahalanobis.distancet <- sqrt(sq.distancet)
plot(sqrt(sq.distancet, labels=DSI$clusters))
mahalanobis.distancet
#Cluster Dendrogram with Complete Linkage
hc.ct <-hclust(distancet) 
#plot(hc.ct, labels=datatset$variable) # plot with labels
plot(hc.ct, hang= -1) # plot dengrogram with all labels ending on the same line
# Cluster Dendogram with Average linkage
hc.at <- hclust(distancet, method = "average")
plot(hc.at, hang= -1) # dengrogram plot with all labels ending on the same line
# kmeans clustering
kc <-kmeans(z,41) # 41 clusters 
kc #defininig cluster inclusion 


## Practice
# Read in data as a CSV file for "Clustering"
clusters <- read.csv(file="Clustering.csv", sep=',', header=T, row.names=1)
dim(clusters) #see summary
print(clusters) #see whole dataset
head(clusters)  # see first 6 entries in dataset
clusters.p <- as.matrix(clusters) # convert to matrix
head(clusters.p)

# create DSI entity
DSI <- clusters$term

# let's try with transposed data
# read-in data
clusters <- read.csv(file="clustertestagain.csv", sep=',', header=T, row.names=1)
clusters.p <- as.matrix(clusters) # convert to matrix
head(clusters) # see first 6 entries in dataset
head (clusters.p) # see first 6 entries in dataset to compare
DSI <- clusters.p$DSI # create DSI Vector

# let's try with and normalized data 
# we will update with term to concept strengths once created
clusters <- read.csv(file="clustertransponsednormalized.csv", sep=',', header=T, row.names=1)
clusters.p <- as.matrix(clusters) # convert to matrix
head(clusters) # see first 6 entries in dataset
head (clusters.p) # see first 6 entries in dataset to compare
sd.clusters <- scale(clusters.p) # scale the data so that terms are equal relative to each term
DSI <- clusters.p$DSI # create DSI Vector
write.csv(sd.clusters, file = "ClusterDataSetCohort3.csv") # This is the Data Set (ST) that I will use for clustering


# We are looking for how many distinct clusters of DSIs exist
# We will build hierarchical clusters using compelete, average and single linkage
par(mfrow = c(3,1)) # display all three heirarchies in one window
data.dist <- dist(sd.clusters) # Distance from center messurments to find clusters
plot(hclust(data.dist), labels = DSI, main = "Complete Linkage", xlab = "", sub = "", ylab = "") # complete linkage
plot(hclust(data.dist, method = "average"), labels = DSI, main = "Average Linkage", xlab = "", sub = "", ylab = "") # average linkage
plot(hclust(data.dist, method = "single"), labels = DSI, main = "Single Linkage", xlab = "", sub = "", ylab = "") # single linkage



## greate clustering template
# Begin EDA
view(dataset) # outputs number of observations of number of variables
str(dataset) # variables by their first 10 observations 

# Scatter Plot
plot(varaible - variable, dataset) # scatter plot of two variables
with(dataset, text(variable, labels=variable)) # label points on the scatterplot by another variable 

# Normalization
z <- dataset[,-c(1,1)] #take out the first column
# so that no variable skews clustering results
m <- apply(z,2,mean) # find mean for all the variables for columns
s <- apply(z,2,sd) # find the standard deviations for columns
z <- scale(z,m,s) # calculate normalized dataset


# Caluclating Euclidean distance
distance <- dist(z) # look for columns and rows with small values indicating high correlation
print(distance, digits=3) # compact view of values

#Cluster Dendrogram with Complete Linkage
hc.c <-hclust(distance) 
plot(hc.c) # plot the dendrogram 
plot(hc.c, labels=datatset$variable) # plot with labels
plot(hc.c, hang= -1) # plot dengrogram with all labels ending on the same line

# Cluster Dendogram with Average linkage
hc.a <- hclust(distance, method = "average")
plot(hc.a, hang= -1) # dengrogram plot with all labels ending on the same line

#Cluster Membership
member.c <- cutree(hc.c,3) # defining membership into 3 clusters using complete linkage
member.a <- cutree(hc.a,3) # defining membership into 3 clusters using average linkage
table(member.c, member.a) # table of how clustering is done using complete vs. average linkage

# cluster means
aggregate(z, list(member.c), mean) #calculates the averge values for each cluster
aggregate(dataset, list(member.c),mean) #calculates the average values for each cluster using the original units for ease of interpretation
# Silhouette Plot
library(cluster) #bring the cluster package into the workspace
plot(silhouette(cutree(hc.c, 3), distance))  # silhouette plot visualizes clusters and rates high and low membership of each variable into cluster 
# uses Euclidean distance and the cutree function

# scree plot
wss <- (nrow(z) -1)*sum(apply(z,2,var)) # within group sum of square errors
for (i in 2:20) wss(i) <- sum(kmeans(z, centers=i)Swithinss) #calculations
plot (1:20, wss, type="b", xlab = "Number of Clusters", ylab= "Within Group SS") # final caluclations
# all possible clusters and within group some of square errors
# look for the drop in variabiility

# kmeans clustering
kc <-kmeans(z,3) # 3 clusters using kmeans
kc #defininig cluster inclusion 
kc$cluster # cluster membership
kc$centers # average values in each cluster
# look at original scatter plot of two variables and define by clsuters.
plot(variable - variable, dataset, col-kc$cluster) # scatter plot of two variables using original values and color by the kmeans clusters

# mahalanobis distance
mahalanobis(dataset, center, cov, inverted = FALSE)





