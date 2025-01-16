#####Random forest model#####

#install.packages("randomForest")
library(randomForest)



set.seed(100)

k <- randomForest(T_stp_heat~T_out+Style+FloorArea+Number_of_Floors+Age_of_Home+Number_of_Occupants, data = All_heat_home, na.action = na.exclude)

####Plot
par(mfrow=c(1,2))

varImpPlot(k, main = 'HSPT when schedule is HOME', cex=1.2, pch=1, col='darkgreen')

imp_heat_home <- varImpPlot(k, main = 'HSPT when schedule is HOME', cex=1.2, pch=1, col='darkgreen')

varImp(k)


#####Decision tree model#### - Very basic model, just to show an example for decision tree in R. The model can be developed further by adding other elements such as confusion matrix, miscalssification error, accuracy

install.packages('party')
library(party)

set.seed(1234) #To get reproducible result

###Data split
ind <- sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData <- dt_data[ind==1,]
testData <- dt_data[ind==2,]

myFormula <- dt_data$cluster ~ dt_data$province + dt_data$Style+dt_data$`Floor Area`+dt_data$`Age of Home`+dt_data$`Auxilliary Heat Fuel Type`

iris_ctree <- ctree(myFormula, data=trainData)

plot(iris_ctree, cex.text=0.8)



#####Correlation plot

# Correlation matrix

library("corrplot")

cormat <- hourly_data_all[c(3:16, 21:33, 36, 38)]####Data selection
M <- cor(cormat)
corrplot(M, method = "circle", tl.cex = 0.6)

