####The below codes are used to process the EV building dataset.


fnames <- list.files()#reading all the file names in the folder
csv <- lapply(fnames, read.csv)#importing the csv files as lists
Consolidated <- do.call(rbind, csv)#consolidating the list as one dataframe

#### a function to merge date and time, if they are separate in the data
Consolidated$DT <- as.POSIXct(as.character(paste(Consolidated$X..Date, Consolidated$Time)), format="%m/%d/%Y %H:%M:%S", tz='EST')#Converting time format


Selected_col <- Consolidated[c(16, 1, 5,6,7)]#selecting required columns for the data analysis


install.packages("dplyr")
library(dplyr)#This library is required for data aggregation
#generating time attributes
Selected_col <- Selected_col %>%
  mutate(Hour= format(Selected_col$DT,"%H"),
         minute= format(Selected_col$DT,"%M"),
         Day = as.numeric(format(Selected_col$DT,"%d")),
         Month = as.numeric(format(Selected_col$DT,"%m")),
         DayOfWeek = weekdays(Selected_col$DT,abbreviate = FALSE))

install.packages("lubridate")
library(lubridate)#This library is required to play with date column
Complete_days <- Selected_col %>%
  mutate(Day_1 = floor_date(DT, unit = "day")) %>%
  group_by(Day) %>%
  mutate(nObservation = n())


New_data <- Complete_days[Complete_days$nObservation == 96,]

## Hourly data
Hourly_data <- New_data %>%
  
  group_by(X..Date, Day, DayOfWeek, Month, Hour) %>%
  
  summarise(Point_3=mean(Point_3),
            Point_4=mean(Point_4),
            Point_5=mean(Point_5))

Hourly_data[c(6:8)] <- round(Hourly_data[c(6:8)], digits = 0)


library('reshape2')#This library is required for transposing the data

Data_transf_Point_3 <- dcast(Hourly_data, X..Date~Hour, value.var = 'Point_3', sum)

## Clustering_k_means
k.max <- 15
data <- Data_transf_Point_3[c(2:25)]
wss1 <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss1
plot(1:k.max, wss1,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "The Elbow method showing the optimal 'k")
box()
abline(v=3, col='red', lty=5)

set.seed(123)

k_means_occupancy <- kmeans(data, centers = 3, nstart = 50)


Centroid_1_2_and_3 <- as.data.frame(k_means_occupancy[["centers"]])

write.csv(Centroid_1_2_and_3, 'Centroid_1_2_and_3.csv')

Data_transf_Point_3$cluster_kmeans <- k_means_occupancy[["cluster"]]

a <- rep(Data_transf_Point_3$cluster, each=24)

Hourly_data$cluster <- a

#season analysis

Func.season <- function(month){
  
  Winter <- c(1,2,3)
  
  Spring <- c(3,4,5)
  
  Summer <- c(6,7,8)
  
  Autumn <- c(9,10,11)
  
  season_division <- c("Winter","Spring", "Summer", "Autumn")
  
  x <- vector()
  
  
  
  for(i in 1:length(month)){
    
    if(month[i] %in% Winter){
      
      x[i] = season_division[1]
      
    }else if(month[i] %in% Spring){
      
      x[i] = season_division[2]
      
    }else if(month[i] %in% Summer){
      
      x[i] = season_division[3]
      
    }else{
      
      x[i] = season_division[4]
      
    }
    
  }
  
  
  
  return(x)
  
}



Hourly_data$Season <- Func.season(Hourly_data$Month)

cluster_1 <- subset(Hourly_data, Hourly_data$cluster==1)

cluster_2 <- subset(Hourly_data, Hourly_data$cluster==2)

cluster_3 <- subset(Hourly_data, Hourly_data$cluster==3)


cluster_1$ID <- rep(1:119, each=24)#Just to create 119 unique ID numbers for each day

cluster_1$ID <- as.factor(cluster_1$ID)

cluster_1$Hour <- as.integer(cluster_1$Hour)


library(ggplot2)
ggplot(cluster_1, aes(x= Hour, y=Point_3, color= ID)) + geom_line() + labs(x = "Time of day", y= "Average Load (kW)")+ ggtitle("Cluster_1") + theme(legend.position = "none")


cluster_2$ID <- rep(1:112, each=24)

cluster_2$ID <- as.factor(cluster_2$ID)

cluster_2$Hour <- as.integer(cluster_2$Hour)


ggplot(cluster_2, aes(x= Hour, y=Point_3, color= ID)) + geom_line() + labs(x = "Time of day", y= "Average Load (kW)")+ ggtitle("Cluster_2") + theme(legend.position = "none")

