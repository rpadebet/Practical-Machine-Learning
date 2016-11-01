library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

#inTrain<-createDataPartition(y =segmentationOriginal$Case,p=0.7,list = FALSE )
training <- segmentationOriginal[segmentationOriginal$Case=="Train",]
testing <- segmentationOriginal[segmentationOriginal$Case=="Test",]

set.seed(125)
modelfit<-train(Class ~ TotalIntenCh2+FiberWidthCh1+PerimStatusCh1+VarIntenCh4,
                method="rpart",data = training)


testset<-data.frame(TotalIntenCh2 = c(23000,50000,57000,0),
                    FiberWidthCh1 = c(10,10,8,8),
                    PerimStatusCh1= c(2,0,0,2),
                    VarIntenCh4 = c(0,100,100,100)
                    )

predict(modelfit$finalModel,testset)

library(rattle)
modelfit$finalModel
fancyRpartPlot(modelfit$finalModel)




#3
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]

inTrain<-createDataPartition(olive$Area,p=0.8,list=FALSE)
training<- olive[inTrain,]
testing<-olive[-inTrain,]

modelTree<-train(Area~.,method="rpart",data=olive)
fancyRpartPlot(modelTree$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
predict(modelTree,newdata)
#####

str(olive)
olive$Area<-as.factor(olive$Area)
summary(olive)

### 
# 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modelfit4<-train(chd~age+alcohol+obesity+tobacco+typea+ldl,method="glm",family="binomial",data=trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd,predict(modelfit4,trainSA))
missClass(testSA$chd,predict(modelfit4,testSA))

#####
# 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

summary(vowel.test)
vowel.test$y<-as.factor(vowel.test$y)
vowel.train$y<-as.factor(vowel.train$y)
summary(vowel.test)
set.seed(33833)

modelfit5<-train(y~.,method="rf",data=vowel.train)

var<-varImp(modelfit5$finalModel)
class(var)
names(var)
order(var,decreasing=TRUE)
