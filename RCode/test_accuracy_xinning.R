library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(MASS)

amzn <-read.csv("AMZN_message.csv")
amzn <-na.omit(amzn)
amzn$MidPrice_Moves = as.factor(amzn$MidPrice_Moves)

train1 = filter(amzn,Time_stamp >= 37800 & Time_stamp <= 39000 & MidPrice_Moves != "2" )
train2 = filter(amzn,Time_stamp >= 37800 & Time_stamp <= 39000 & MidPrice_Moves == "2")
set.seed(144)
train.ids = sample(nrow(train2), 0.43*nrow(train2))
train3 = train2[train.ids,]
train = rbind(train1,train3)

test = filter(amzn, Time_stamp > 39000 & Time_stamp <= 39001.7)
count(test,MidPrice_Moves)
#              0     2
#              1     8
#              2    40


#lda accuracy 84%
lda1 <-lda(MidPrice_Moves~.-Time_stamp-Time-Label,data=train)
predlda <- predict(lda1, newdata = test, type = "class")
confusionMatrix(test$MidPrice_Moves,predlda$class)
#Reference
#Prediction  0  1  2
#0           1  1  0
#1           1  6  1
#2           2  3 35


#CART accuracy 88%
#predcart
#   0  1  2
#0  2  0  0
#1  1  7  0
#2  1  4 35
cpVals = data.frame(cp = seq(0, .5, by=.01))
train.cart.st <- train(MidPrice_Moves~.-Time_stamp-Time-Label,
                       data = train,
                       method = "rpart",
                       tuneGrid = cpVals,
                       trControl = trainControl(method = "cv", number=5),
                       metric = "Accuracy")
predcart <- predict(train.cart.st, newdata = test, type = "class")
table(test$MidPrice_Moves, predcart)

#random forest 88%
#0  1  2 
#2 11 37 
mod.rf <- randomForest(MidPrice_Moves~.-Time_stamp-Time-Label, data = train, mtry = 5, nodesize = 5, ntree = 500)
pred.rf <- predict(mod.rf, newdata = test) 

#boosting
mod.boost <- gbm(MidPrice_Moves~.-Time_stamp-Time-Label,
                 data = train,
                 distribution = "gaussian",
                 n.trees = 10,
                 shrinkage = 0.001,
                 interaction.depth = 2)
pred.boost <- predict(mod.boost, newdata = test,n.trees=10,type="response")
table(test$MidPrice_Moves, pred.boost)