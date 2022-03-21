creditcard <- read.csv("D1.2 Credit card defaults.csv")

#generate random training/validation index
set.seed(2)

train_size = floor(nrow(creditcard)*.70)
train_index = sample(seq_len(nrow(creditcard)),size = train_size)
test_index = -train_index

train <- creditcard[train_index,]
test <- creditcard[test_index,]

#generate 2 logistic regression modesl

#model 1: simplest with at least 5 explanatory variables
glm.fit1 = glm(defaultpaymentnextmonth~limit_bal + sex + education + marriage + age, 
               family=binomial, data=train)
summary(glm.fit1)

#model 2: slightly more complex model
glm.fit2 = glm(defaultpaymentnextmonth~limit_bal + sex + education + marriage + age + pay_1 + 
                 pay_2 + bill_amt1 + bill_amt2 + pay_amt1 + pay_amt2 + pay_1:pay_amt1, 
               family=binomial, data=train)
summary(glm.fit2)


#classification
#predict the probability that the one will default, for each person in the testing data.
glm1.probs=predict(glm.fit1,test,type="response")
glm2.probs=predict(glm.fit2,test,type="response")
#create a vector glm1.preds/glm2.preds, which initially consists of all 30000 observations
glm1.preds = rep(0,30000)
glm2.preds = rep(0,30000)
#update vector to yes with threshold 0.5
glm1.preds[glm1.probs>.5] = 1
glm2.preds[glm2.probs>.5] = 1

#create a table, which cross-tabulates my predictions (in glm.pred vector)
#with respect to actual defaultpaymentnextmonth
table(glm1.preds,creditcard$defaultpaymentnextmonth)
table(glm2.preds,creditcard$defaultpaymentnextmonth)
# compute the percentage of time we were correct
mean(glm1.preds == creditcard$defaultpaymentnextmonth)
mean(glm2.preds == creditcard$defaultpaymentnextmonth)

install.packages("pROC")
library("pROC")

#ROC,AUC of the two models
roc(test$defaultpaymentnextmonth~glm1.probs,plot=TRUE,print.auc=TRUE)
roc(test$defaultpaymentnextmonth~glm2.probs,plot=TRUE,print.auc=TRUE)
