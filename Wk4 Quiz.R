AppliedPredictiveModeling: v1.1.6

caret: v6.0.47

ElemStatLearn: v2012.04-0

pgmm: v1.1

rpart: v4.1.8

gbm: v2.1

lubridate: v1.3.3

forecast: v5.6

e1071: v1.6.4

require(devtools)
install_version("AppliedPredictiveModeling", version = "1.1.6", repos = "http://cran.us.r-project.org")
install_version("caret", version = "6.0.47", repos = "http://cran.us.r-project.org")
require(AppliedPredictiveModeling)
require(caret)
require(ElemStatLearn)
require(rpart)
require(lubridate)
require(e1071)

install.packages("gbm")
require(gbm)
install.packages("forecast")
require("forecast")



## 1st question
data("vowel.train")
data("vowel.test")

load(vowel.train)
load(vowel.test)

str(vowel.train)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)
modelrf<-train(y~.,method="rf",data=vowel.train)
modelgbm<-train(y~.,method="gbm",data=vowel.train)

summary(modelrf)
summary(modelgbm)

predrf<-predict(modelrf,newdata = vowel.test)
predgbm<-predict(modelgbm,newdata =vowel.test)

confusionMatrix(vowel.test$y,predrf)
#0.6147
confusionMatrix(vowel.test$y,predgbm)
#0.5368

pred.agree<-ifelse(predrf==predgbm,predrf,NA)
confusionMatrix(vowel.test$y,pred.agree)
#0.6656

## 2 question

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

model.rf<-train(diagnosis~.,model="rf",data=training,metric="Accuracy")
model.gbm<-train(diagnosis~.,model="gbm",data=training,metric="Accuracy")
model.lda<-train(diagnosis~.,model="lda",data=training,metric="Accuracy")

# summarize results
bagging_results <- resamples(list(rf=model.rf, gbm=model.gbm,lda=model.lda))
summary(bagging_results)
dotplot(bagging_results)

# stacking
install.packages("caretEnsemble")
require(caretEnsemble)
algorithmList <- c('rf','gbm','lda')
models <- caretList(diagnosis~., data=training, methodList=algorithmList,trControl=trainControl(classProbs = TRUE))
results <- resamples(models)
summary(results)
stack.rf <- caretStack(models, method="rf", metric="Accuracy")
print(stack.rf)

# Predictions
pred.rf<-predict(model.rf,testing)
confusionMatrix(testing$diagnosis,pred.rf)$overall[1]
pred.gbm<-predict(model.gbm,testing)
confusionMatrix(testing$diagnosis,pred.gbm)$overall[1]
pred.lda<-predict(model.lda,testing)
confusionMatrix(testing$diagnosis,pred.lda)$overall[1]

# combined model
data.comb<-data.frame(rf=pred.rf,gbm=pred.gbm,lda=pred.lda,diagnosis=testing$diagnosis)
model.combo.rf<-train(diagnosis~.,data=data.comb,method="rf")
pred.combo<-predict(model.combo.rf,data.comb)
confusionMatrix(testing$diagnosis,pred.combo)$overall[1]


## Question 3

set.seed(3523)

library(AppliedPredictiveModeling)
install.packages("elasticnet")
library(elasticnet)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
model.lasso<-train(CompressiveStrength~.,model="lasso",data=training)
plot(model.lasso,plottype="coefficients")
summary(model.lasso)
model.lasso
model.lasso$finalModel
model.lasso$coefnames



model.lasso.enet<-enet(x=as.matrix(training[,-c(9)]),y=training$CompressiveStrength,lambda = 0)
plot.enet(model.lasso.enet,xvar="penalty",use.color = TRUE,label=TRUE)

data(diabetes)
attach(diabetes)
object <- enet(x,y,lambda=1)
par(mfrow=c(1,1))
plot(object)
plot(object,xvar="penalty")
detach(diabetes)



### question 5

set.seed(3523)

library(AppliedPredictiveModeling)
library(e1071)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]
set.seed(325)
model.svm=svm(CompressiveStrength ~ .,data=training)
print(model.svm)
summary(model.svm)
predict.svm.tr = predict(model.svm,training)
accuracy(training$CompressiveStrength,predict.svm.tr)

predict.svm = predict(model.svm,testing)
accuracy(testing$CompressiveStrength,predict.svm)


### question 4
url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"

download.file(url,destfile = "gaData.csv",method = "curl")
library(lubridate)
dat<-read.csv("gaData.csv",stringsAsFactors = FALSE)

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)

visit.fit<-bats(tstrain)
plot(forecast(visit.fit,h=nrow(testing),level=c(95)))
print(visit.fit)

plot(tstrain)
lines(visit.fit$fitted,col="red")

plot(visit.fit)
accuracy(visit.fit)


res<-forecast.bats(visit.fit,h=nrow(testing),level = c(95))
plot(ts(testing$visitsTumblr))
lines(res$mean,col="red")
lines(res$upper,col="blue")
lines(res$lower,col ="blue")

high<-as.vector(NULL)
high<-sum(testing$visitsTumblr>=res$upper)
low<-sum(testing$visitsTumblr<=res$lower)

#points within conf int
1- ((high+low)/nrow(testing))
