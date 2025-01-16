####Make sure to install the following packages before using them for k-shape clustering. To install a package write for example, 
#install.packages(TSrepe)
#Once you installed the package, to use the specific package, it is important to call the library. for example, library(TSrepr)
#To perfom the k-shape clustering, following libraries are required. 


library('TSrepr')
library('ggplot2')
library('data.table')
library('cluster')
library('clusterCrit')

library('dtwclust').

library('TSclust')

library('dplyr')

library('microbenchmark')


library('reshape2')


library('pheatmap')

library('gplots')

library('RColorBrewer')

#the example shown here is suitable for timeseries data. The data must be in matrix type.

data_matrix <- as.matrix(data_with_Dayindex[c(2:25)]) ### (2:25) denotes the column that has 24 hr data in the dataset

##For finding the optimal number of clusters

clusterings_wd_Type_1_c1 <- lapply(c(2:10), function(x)
  pam((as.matrix(data_matrix)), x))


##Dunn index is used as example. We can use Silhoute or Davies-Bouldin as per requirement
DB_values_wd_Type_1_c1 <- sapply(seq_along(clusterings_wd_Type_1_c1), function(x) 
  intCriteria(data_matrix, as.integer(clusterings_wd_Type_1_c1[[x]]$clustering),
              c("Dunn")))


###Plot to visulaizw the optimal number of clusters
ggplot(data.table(Clusters = 2:10, Dunn_index = unlist(DB_values_wd_Type_1_c1)),
       aes(Clusters, Dunn_index)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw() + ggtitle("CVI")

###Time series data - kshape clustering
dtw_cluster_wd_type_1_c1 = tsclust(data_matrix, type="partitional",k=3, preproc = zscore,seed=110,
                                   distance="sbd",centroid = "shape",trace=T, na.rm=TRUE)

###Plots 
plot(dtw_cluster_wd_type_1_c1)

plot(dtw_cluster_wd_type_1_c1, type = "series", clus = 2L)
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 1L)+xlab('Hours')+ylab('Z-normalized value')+ggtitle("Cluster 1")
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 2L)+xlab('Hours')+ylab('Z-normalized value')+ggtitle("Cluster 2")
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 3L)+xlab('Hours')+ylab('Z-normalized value')+ggtitle("Cluster 3")


####Adding the cluster number in your original data
data_with_Dayindex$cluster <- dtw_cluster_wd_type_1_c1@cluster


##Exporting data as an csv file (optional)

write.csv(Type_1_data_with_Dayindex, 'Type_1_data_with_Dayindex.csv')


###Filtering the data based on clusters

cluster_1 <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==1)

cluster_1 <- cluster_1[-c(46, 51, 52), ] #(Removing occupant movements with 60 and 120)


####Heat map

library(grid)

colors<-colorRampPalette(rev(brewer.pal(n=7,name="Spectral")))(7)

cluster_1 <- cluster_1[-c(46, 51, 52), ]
pheatmap::pheatmap(as.matrix(cluster_1[c(2:25)]), treeheight_row = 0, treeheight_col = 0,
                   cluster_rows=F, cluster_cols=F, col=colors, legend = TRUE, legend_labels = 'No.of.occupants', main = 'Cluster 1', xlab = 'Hour', cex=1.2)


cluster_2 <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==2)

cluster_2 <- cluster_2[-c(127), ]


pheatmap::pheatmap(as.matrix(cluster_2[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F, col=colors, main = 'Cluster 2', cex=1.2)


cluster_3 <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==3)

cluster_3 <- cluster_3[-c(85), ]


pheatmap::pheatmap(as.matrix(cluster_3[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F, col=colors, main = 'Cluster 3', cex=1.2)


#####From here on it is optional. Codes to plot specific figures in each cluster


#Cluster_1_mean_profile_hourly_analysis

Cluster_1_mean_profile_type_1 <- cluster_1_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

Cluster_1_mean_profile_type_1[c(2,3,4,5,6)] <- round(Cluster_1_mean_profile_type_1[c(2,3,4,5,6)], digits = 0)

plot(x=Cluster_1_mean_profile_type_1$Hour, y=Cluster_1_mean_profile_type_1$CO2_avg, xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_1', type='l')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


#M_Cluster_2_mean_profile_hourly_analysis

Cluster_2_mean_profile_type_1 <- cluster_2_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

Cluster_2_mean_profile_type_1[c(2,3,4,5,6)] <- round(Cluster_2_mean_profile_type_1[c(2,3,4,5,6)], digits = 0)

plot(x=Cluster_2_mean_profile_type_1$Hour, y=Cluster_2_mean_profile_type_1$CO2_avg, type='l',xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_2')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


#M_Cluster_3_mean_profile_hourly_analysis

Cluster_3_mean_profile_type_1 <- cluster_3_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

Cluster_3_mean_profile_type_1[c(2,3,4,5,6)] <- round(Cluster_3_mean_profile_type_1[c(2,3,4,5,6)], digits = 0)

plot(x=Cluster_3_mean_profile_type_1$Hour, y=Cluster_3_mean_profile_type_1$CO2_avg, type='l',xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_3')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


## Daily analysis_Cluster_1 ##

Cluster_1_Monday <- filter(cluster_1_OD, Day == 'Monday')

Cluster_1_Monday_mean_profile <- Cluster_1_Monday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Monday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Monday_mean_profile[c(2,3,4,5,6)], digits = 0)

#Tuesday
Cluster_1_Tuesday <- filter(cluster_1_OD, Day == 'Tuesday')

Cluster_1_Tuesday_mean_profile <- Cluster_1_Tuesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Tuesday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Tuesday_mean_profile[c(2,3,4,5,6)], digits = 0)


#Wednesday
Cluster_1_Wednesday <- filter(cluster_1_OD, Day == 'Wednesday')

Cluster_1_Wednesday_mean_profile <- Cluster_1_Wednesday %>%
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Wednesday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Wednesday_mean_profile[c(2,3,4,5,6)], digits = 0)

#Thursday
Cluster_1_Thursday <- filter(cluster_1_OD, Day == 'Thursday')

Cluster_1_Thursday_mean_profile <- Cluster_1_Thursday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot),CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Thursday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Thursday_mean_profile[c(2,3,4,5,6)], digits = 0)


#Friday

Cluster_1_Friday <- filter(cluster_1_OD, Day == 'Friday')

Cluster_1_Friday_mean_profile <- Cluster_1_Friday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot),CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Friday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Friday_mean_profile[c(2,3,4,5,6)], digits = 0)

par(mfrow=c(1,3))

plot(Cluster_1_Monday_mean_profile$P_tot_wd, type='l', col='blue', ylim=c(0,70), ylab='Occupant movements detected', xlab='Hours', lwd=3, main ='Cluster 1', cex.axis=2, cex.lab=2, cex.main=3)
#,xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_1_Tuesday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_1_Wednesday_mean_profile$M_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_1_Thursday_mean_profile$M_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_1_Friday_mean_profile$M_tot_wd, type='l', col='black', lwd=3)
legend(
  "topleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)


## Daily analysis_Cluster_2 ##

Cluster_2_Monday <- filter(cluster_2_OD, Day == 'Monday')

Cluster_2_Monday_mean_profile <- Cluster_2_Monday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Monday_mean_profile[c(2,3,4)] <- round(Cluster_2_Monday_mean_profile[c(2,3,4)], digits = 0)


#Tuesday
Cluster_2_Tuesday <- filter(cluster_2_OD, Day == 'Tuesday')

Cluster_2_Tuesday_mean_profile <- Cluster_2_Tuesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Tuesday_mean_profile[c(2,3,4)] <- round(Cluster_2_Tuesday_mean_profile[c(2,3,4)], digits = 0)


#Wednesday
Cluster_2_Wednesday <- filter(cluster_2_OD, Day == 'Wednesday')

Cluster_2_Wednesday_mean_profile <- Cluster_2_Wednesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Wednesday_mean_profile[c(2,3,4)] <- round(Cluster_2_Wednesday_mean_profile[c(2,3,4)], digits = 0)

#Thursday
Cluster_2_Thursday <- filter(cluster_2_OD, Day == 'Thursday')

Cluster_2_Thursday_mean_profile <- Cluster_2_Thursday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Thursday_mean_profile[c(2,3,4)] <- round(Cluster_2_Thursday_mean_profile[c(2,3,4)], digits = 0)


#Friday

Cluster_2_Friday <- filter(cluster_2_OD, Day == 'Friday')

Cluster_2_Friday_mean_profile <- Cluster_2_Friday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Friday_mean_profile[c(2,3,4)] <- round(Cluster_2_Friday_mean_profile[c(2,3,4)], digits = 0)



plot(Cluster_2_Monday_mean_profile$M_tot_wd, type='l', col='blue', ylim=c(0, 70), ylab='Occupant movements detected', lwd=3, main='Cluster 2', xlab='Hours',cex.axis=2, cex.lab=2, cex.main=3)
#, xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_2_Tuesday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_2_Wednesday_mean_profile$M_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_2_Thursday_mean_profile$M_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_2_Friday_mean_profile$M_tot_wd, type='l', col='black', lwd=3)
legend(
  "topright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)


## Daily analysis_Cluster_3 ##

Cluster_3_Monday <- filter(cluster_3_OD, Day == 'Monday')

Cluster_3_Monday_mean_profile <- Cluster_3_Monday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Monday_mean_profile[c(2,3,4)] <- round(Cluster_3_Monday_mean_profile[c(2,3,4)], digits = 0)

#Tuesday
Cluster_3_Tuesday <- filter(cluster_3_OD, Day == 'Tuesday')

Cluster_3_Tuesday_mean_profile <- Cluster_3_Tuesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Tuesday_mean_profile[c(2,3,4)] <- round(Cluster_3_Tuesday_mean_profile[c(2,3,4)], digits = 0)


#Wednesday
Cluster_3_Wednesday <- filter(cluster_3_OD, Day == 'Wednesday')

Cluster_3_Wednesday_mean_profile <- Cluster_3_Wednesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Wednesday_mean_profile[c(2,3,4)] <- round(Cluster_3_Wednesday_mean_profile[c(2,3,4)], digits = 0)

#Thursday
Cluster_3_Thursday <- filter(cluster_3_OD, Day == 'Thursday')

Cluster_3_Thursday_mean_profile <- Cluster_3_Thursday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Thursday_mean_profile[c(2,3,4)] <- round(Cluster_3_Thursday_mean_profile[c(2,3,4)], digits = 0)


#Friday

Cluster_3_Friday <- filter(cluster_3_OD, Day == 'Friday')

Cluster_3_Friday_mean_profile <- Cluster_3_Friday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Friday_mean_profile[c(2,3,4)] <- round(Cluster_3_Friday_mean_profile[c(2,3,4)], digits = 0)



plot(Cluster_3_Monday_mean_profile$M_tot_wd, type='l', col='blue', ylim=c(0, 70), ylab='Occupant movements detected', xlab='Hours', main='Cluster 3', lwd=3, cex.axis=2, cex.lab=2, cex.main=3)
#,xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_3_Tuesday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_3_Wednesday_mean_profile$M_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_3_Thursday_mean_profile$M_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_3_Friday_mean_profile$M_tot_wd, type='l', col='black', lwd=3)
legend(
  "topleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)


#####Change point analysis 

library(changepoint)

### CPA ####

##Test the cpt_evaluation function and cpt_count function
Cluster_1_CPA_Jun <- cpt_evaluation(cluster_1_OD[c(11,6)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')


Cluster_1_CPA_RF <- subset(Cluster_1_CPA_Jun, select = c(2:4))


rel_freq_daily <- table(Cluster_1_CPA_RF)

freq_daily <- table(Cluster_1_CPA_RF)/length(Cluster_1_CPA_RF)

freq_daily <- as.data.frame(freq_daily)

barplot(freq_daily, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "orange", main = "Cluster_1 - Weekdays - Type-1", las=2) 
box()

write.csv(freq_daily, 'freq_daily.csv')

##Test the cpt_evaluation function and cpt_count function
Cluster_2_CPA_Jun <- cpt_evaluation(cluster_2_OD[c(11,6)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')


Cluster_2_CPA_RF <- subset(Cluster_2_CPA_Jun, select = c(2:4))


rel_freq_daily_Clus_2 <- table(Cluster_2_CPA_RF)

freq_daily_Clus_2 <- table(Cluster_2_CPA_RF)/length(Cluster_2_CPA_RF)

barplot(freq_daily_Clus_2, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "green", main = "Cluster_2 - Weekdays - Type-1", las=2) 
box()

write.csv(freq_daily_Clus_2, 'freq_daily_clus_2.csv')

##Test the cpt_evaluation function and cpt_count function
Cluster_3_CPA_Jun <- cpt_evaluation(cluster_3_OD[c(11,6)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')


Cluster_3_CPA_RF <- subset(Cluster_3_CPA_Jun, select = c(2:4))

rel_freq_daily_Clus_3 <- table(Cluster_3_CPA_RF)

freq_daily_Clus_3 <- table(Cluster_3_CPA_RF)/length(Cluster_3_CPA_RF)

barplot(freq_daily_Clus_3, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "yellow", main = "Cluster_3 - Weekdays - Type-1", las=2) 
box()
abline(h=0.06)

write.csv(freq_daily_Clus_3, 'freq_daily_clus_3.csv')


CPA_cluster_3_mean <- cpt.mean(Cluster_3_mean_profile_type_1$M_tot_wd,penalty = "AIC", Q=6, method="BinSeg")
cpts(CPA_cluster_3_mean)
plot(CPA_cluster_3_mean, xlab="Time(min)",ylab="Occupant probability", main ='Occupancy probability Friday - MeanVariance', xaxt='n')
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
abline(v=c(6, 7,12,16,19,21))



