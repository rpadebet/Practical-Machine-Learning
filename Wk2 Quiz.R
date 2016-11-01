install.packages("AppliedPredictiveModeling")
install.packages("caret")
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


library(ggplot2)
library(Hmisc)
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(inTrain,training$CompressiveStrength,color=training$FlyAsh)
    training$FlyAshF <- cut2(training$FlyAsh,g = 2)
qplot(inTrain,training$CompressiveStrength,color=training$FlyAshF)

training$AgeF <- cut2(training$Age,g=5)
qplot(inTrain,training$CompressiveStrength,color=training$AgeF)

qplot(inTrain,training$CompressiveStrength,color=training$Age)

hist(training$Superplasticizer,breaks=15)
hist(log(training$Superplasticizer)+1,breaks = 15)

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

colAd<-names(training)
meanIdx <- grep(pattern = "^IL",as.character(colAd),value = FALSE)
sel<-colAd[meanIdx]
sel_new<-gsub("_",replacement = ".",sel)


train_sel<-training[,c(sel,"diagnosis")]
names(train_sel)<-c(sel_new,"diagnosis")

test_sel<-testing[,c(sel,"diagnosis")]
names(test_sel)<-c(sel_new,"diagnosis")

prComp<-prcomp(train_sel[,-13])
summary(prComp)

var<-(prComp$sdev)/sum((prComp$sdev))

plot(cumsum(var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
abline(h = 0.8,col="red")



preProc<-preProcess(x = train_sel[,-13],method = "pca",pcaComp = 3)
trainPC<-predict(preProc,train_sel[,-13])
trainPC$diagnosis<-train_sel$diagnosis
testPC<-predict(preProc,test_sel[,-13])
testPC$diagnosis<-test_sel$diagnosis

modelfitPC<-train(diagnosis~., method="glm",data=trainPC)
confusionMatrix(test_sel$diagnosis,predict(modelfitPC,testPC))


modelfit<-train(diagnosis~., method="glm",data=train_sel)
confusionMatrix(test_sel$diagnosis,predict(modelfit,test_sel))


