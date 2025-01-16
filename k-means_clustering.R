###### Kmeans clustering in R####

####setting maximum number of clusters. This can be a choice of the user

k.max <- 15

##data
data <- Total_data_for_k_means[c(2:25)]

####Finding the optimal number of clusters using elbow method. We can also Dunn index, Silhouette and other cluster validation indices
wss1 <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss1
plot(1:k.max, wss1,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movement_Morning period")
box()


##### kmeans clustering#####
set.seed(123)


k_means_occupancy <- kmeans(data, centers = 3, nstart = 50)

###Getting the centroids of each cluster
Centroid_1_2_and_3 <- k_means_occupancy[["centers"]]

###Assigning the cluster number to the original dat, so that the data corresponding to each cluster can be filtered and analyzed separately. 

Total_data_for_k_means$cluster_kmeans <- k_means_occupancy[["cluster"]]

write.csv(Total_data_for_k_means, 'Total_data_for_k_means_with_cluster_details.csv')

#Cluster validation index (other than Elbow method)
library(clValid)
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(data, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
summary(intern)

plot(intern)
