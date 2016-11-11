# The training data for this project are available here:
    trainingurl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(trainingurl,"pml-training.csv",method="curl")

# The test data are available here:
    testingurl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(testingurl,"pml-testing.csv",method="curl")
    
# Load the data into R
    training<-read.csv("pml-training.csv",stringsAsFactors = FALSE)
    testing<-read.csv("pml-testing.csv",stringsAsFactors = FALSE)
    
    training$classe=as.factor(training$classe)
    training$user_name=as.factor(training$user_name)
    training$new_window=as.factor(training$new_window)
    
    
# Exploring the data
    str(training)
    dim(training)
    summary(training)

# Cleaning Data       
# Removing cols with mostly NA's
   sum(is.na(training$min_roll_belt))
   NACols<-apply(training,MARGIN = 2,FUN = function(X) sum(is.na(X)))
   training_sub<-training[,names(NACols[NACols<1000])]
   str(training_sub)
   
 # Removing cols with mostly blank characters
   sum(training$max_yaw_belt=="")
   BlnkCols<-apply(training,MARGIN = 2,FUN = function(X) sum(X==""))
   BCols<-BlnkCols[!is.na(BlnkCols)]
   training_set<-training_sub[,names(BCols[BCols<1000])]
   str(training_set)
   
 # Removing raw timestamp columns

 # Removing same cols from testing set
   dim(testing)
   testing_set<-subset(testing,select = names(training_set[,-c(60)]))
   names(training)==names(testing)
   
 # Creating own training and testing sets
   set.seed(111016)
   inTrain = createDataPartition(training_set$classe, p = 3/4)[[1]]
   p_training = training_set[ inTrain,]
   p_testing = training_set[-inTrain,]

# Modeling
   set.seed(111016)
   library(caret)
   library(gbm)
   library(C50)
   library(doParallel)
   trainctrl<-trainControl(method="repeatedcv"
                           ,number=10,repeats = 5
                           ,verboseIter = FALSE
                           ,allowParallel = TRUE)
   preprocess<-c("center", "scale")
   
   cl <- makeCluster(detectCores()-1)
   registerDoParallel(cl)
  
  
   # Random Forest Model
   model.rf<-train(form = classe~.,
                   model = "rf",
                   data = p_training,
                   metric="Accuracy",
                   preProcess = preprocess,
                   trControl = trainctrl,
                   verbose=TRUE
                   )
   
   # Gradient Boosting Model
   model.gbm<-train(form = classe~.,
                   model = "gbm",
                   data = p_training,
                   metric="Accuracy",
                   preProcess = preprocess,
                   trControl = trainctrl,
                   verbose=TRUE
   )
  
   # Linear Discriminant Analysis
   model.lda<-train(form = classe~.,
                    model = "lda",
                    data = p_training,
                    metric="Accuracy",
                    preProcess = preprocess,
                    trControl = trainctrl,
                    verbose=TRUE
   )
   
   # summarize results
   bagging_results <- resamples(list(rf=model.rf, gbm=model.gbm,lda=model.lda))
   summary(bagging_results)
   dotplot(bagging_results)
   
   # Predictions
   pred.rf<-predict(model.rf,p_testing)
   confusionMatrix(p_testing$classe,pred.rf)$overall[1]
   pred.gbm<-predict(model.gbm,p_testing)
   confusionMatrix(p_testing$classe,pred.gbm)$overall[1]
   pred.lda<-predict(model.lda,p_testing)
   confusionMatrix(p_testing$classe,pred.lda)$overall[1]
   
   # combined model using C5.0
   data.comb<-data.frame(rf=pred.rf,gbm=pred.gbm,lda=pred.lda,classe=p_testing$classe)
   
   model.combo<-train(form = classe~.,
                         model = "C5.0",
                         data = data.comb,
                         metric="Accuracy",
                         preProcess = preprocess,
                         trControl = trainctrl,
                         verbose=TRUE
   )
   pred.combo<-predict(model.combo,data.comb)
   stopCluster(cl)
   confusionMatrix(p_testing$classe,pred.combo)$overall[1]
  