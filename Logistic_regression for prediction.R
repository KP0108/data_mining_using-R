######Logistic regression___________

#Data partition

set.seed(1234)### This is a method to get the same results everytime the code is run. If the seed is not set, we will get different answers each time

ind_new <- sample(2, nrow(Data), replace = T,  prob = c(0.75,0.25))
train_new <- Data[ind_new==1,]
test_new <- Data[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~Avg_RH+Temp+load, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)

optimalCutoff(train_new$M_tot, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.75, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

#####optimal cutoff is a method to increa

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.74, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

plotROC(test_new$M_tot, p2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))


