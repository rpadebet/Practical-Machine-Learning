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
    
    
# Exploring the data
    dim(training)
    str(training)
    
    dim(testing)
    str(testing)
    

# Cleaning Data       

 # Removing cols with mostly NA's
    
   NACols<-apply(training,MARGIN = 2,FUN = function(X) sum(is.na(X)))
   training_sub<-training[,names(NACols[NACols<1000])]
   
 # Removing cols with mostly blank characters
   
   BlnkCols<-apply(training,MARGIN = 2,FUN = function(X) sum(X==""))
   BCols<-BlnkCols[!is.na(BlnkCols)]
   training_set<-training_sub[,names(BCols[BCols<1000])]
   
 # Removing raw timestamp and name columns
   remCols<-c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2",
              "cvtd_timestamp","new_window","num_window")
   training_set<-training_set[,!(names(training_set) %in% remCols)]

 # Removing same cols from testing set

   testing_set<-subset(testing,select = c(names(training_set[,-c(53)]),"problem_id"))
   
 # Creating own training and testing sets
   set.seed(1234)
   inTrain = createDataPartition(training_set$classe, p = 3/4)[[1]]
   p_training = training_set[ inTrain,]
   p_testing = training_set[-inTrain,]

   # Trying model search
   mod1<-lm(classe~.,data = p_training)
   summary(mod1)$coeff
   search<-step(mod1,direction = "backward",trace = TRUE)
   search$anova
# Modeling
   set.seed(1234)
   library(caret)
   library(doParallel)
   
   trainctrl<-trainControl(method="repeatedcv"
                           ,number=5,repeats = 2
                           ,verboseIter = TRUE
                           ,allowParallel = TRUE
                           )
   
   preprocess<-c("center", "scale","nzv")
  # myGrid<-expand.grid(mtry=seq(7,30,7))
   cl <- makeCluster(detectCores()-1)

   
   # Random Forest Model
   registerDoParallel(cl)
   system.time(
   model.fit<-train(form = classe ~ .,
                   method = "rf",
                   data = p_training,
                   metric="Accuracy",
                   preProcess = preprocess,
                   trControl = trainctrl,
                   adaptive = list(min = 10, 
                                   alpha = 0.05, 
                                   method = "gls",
                                   complete = TRUE),
                   #tuneLength=15
                   #tuneGrid = myGrid,
                   verbose=TRUE
   )
   )
   stopCluster(cl)
   summary(model.fit)
   print(model.fit)
   plot(model.fit)
   plot(model.fit$finalModel)
   model.fit$finalModel$confusion
   
   # Prediction using our testing set
  
   pred.fit<-predict(model.fit,p_testing)
   confusionMatrix(p_testing$classe,pred.fit)
   
   
   # Prediction using the provided testing set
   pred.test.fit<-predict(model.fit,testing_set)
   summary(pred.test.fit)
   
   testing_pred<-cbind(testing_set,pred.test.fit)
   View(testing_pred[,c(53,54)])
   
   trainctrlg<-trainControl(method="repeatedcv"
                           ,number=5,repeats = 3
                           )
   
   library("gbm")
   preprocess<-c("center", "scale","nzv","corr")
   myGbm<-expand.grid(n.trees = seq(200,1000,100),interaction.depth = 2:4,
                      shrinkage = 0.05,n.minobsinnode=20)
   registerDoParallel(cl)
   system.time(
       model.gbm<-train(form = classe ~ .,
                        method = "gbm",
                        data = p_training,
                        metric="Accuracy",
                        preProcess = preprocess,
                        trControl = trainctrlg,
                        tuneGrid = myGbm
       )
   )
   stopCluster(cl)
   summary(model.gbm)
   print(model.gbm)
   plot(model.gbm)
   plot(model.gbm$finalModel)
   print(model.gbm$finalModel)
   
   saveRDS(model.gbm,"gbmmodel2.rds")
   
   
   dotPlot(varImp(model.gbm))
   confusionMatrix(predict(model.gbm,p_training),p_training$classe)
   # Prediction using our testing set
   
   pred.gbm<-predict(model.gbm,p_testing)
   confusionMatrix(p_testing$classe,pred.gbm)
   
   
   # Prediction using the provided testing set
   pred.test.gbm<-predict(model.gbm,testing_set)
   summary(pred.test.gbm)
   testing_pred<-cbind(testing_set,pred.test.gbm)
   View(testing_pred[,c(53,54)])
   
 
   
 
   
  