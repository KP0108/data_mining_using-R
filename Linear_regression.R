#######Linear regression#######

#####Pairplot to identify the correlation between different attributes in the data


###selecting the data
pairs(Daily_data[c(2,4:8)], col=Daily_data$Movements) ###Finding the correlation between one attribute and the others. Here the correlation is found between the occupant movements and other attributes. 


###Pairplot
pairs(Cluster_1_mean_profile_type_1[2:4], col=Cluster_1_mean_profile_type_1$`Number of Occupant movements`, pch=19)



##Multiple linear regression model

lm1 <- lm(PM2.5_Concentration  ~Pot_Temperature+Spec_Humidity+PM2.5_DeposedMass+Wind_Speed, data = Shima_Try)

summary(lm1)

plot(lm1)

plot(predict(lm1), Shima_Try$PM2.5_Concentration, xlim=c(89.5, 89.6))
#Regression line
abline(a=0, b=1)

library(MASS)
cc <- lm1$coefficients

###Getting regression equation#####
(eqn <- paste("Y =", paste(round(cc[1],3), paste(round(cc[-1],4), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e"))

confint(lm1)

library(Metrics)

#### Error metrics. We can calculate GIni, Precision, RMSE, F1-Score, AUC, MAE, MAPE, MSE, etc. using this library

mape((lm1$fitted.values), Shima_Try$PM2.5_DeposedMass)

