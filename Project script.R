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
   
 # Removing raw timestamp and name columns
   remCols<-c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2",
              "cvtd_timestamp","new_window")
   training_set<-training_set[,!(names(training_set) %in% remCols)]

 # Removing same cols from testing set
   dim(testing)
   testing_set<-subset(testing,select = c(names(training_set[,-c(54)]),"problem_id"))

   str(testing_set)
   
 # Creating own training and testing sets
   set.seed(1234)
   inTrain = createDataPartition(training_set$classe, p = 3/4)[[1]]
   p_training = training_set[ inTrain,]
   p_testing = training_set[-inTrain,]

# Modeling
   set.seed(1234)
   library(caret)
   library(gbm)
   library(C50)
   library(doParallel)
   trainctrl<-trainControl(method="repeatedcv"
                           ,number=10,repeats = 3
                           ,verboseIter = FALSE
                           ,allowParallel = TRUE)
   
   # preprocess<-c("center", "scale")
   # myTuneGrid <- expand.grid(n.trees = 1:100,interaction.depth = 2:4,shrinkage = 0.1)
   
   cl <- makeCluster(detectCores()-1)
   registerDoParallel(cl)
   
   # Gradient Boosting Model
   registerDoParallel(cl)
   model.gbm<-train(form = classe~.,
                   model = "gbm",
                   data = p_training,
                   metric="Accuracy",
                   #preProcess = preprocess,
                   #tuneGrid = myTuneGrid,
                   trControl = trainctrl,
                   verbose=TRUE
   )
   stopCluster(cl)
   summary(model.gbm)
   print(model.gbm)
   
   # Prediction using our testing set
  
   pred.gbm<-predict(model.gbm,p_testing)
   confusionMatrix(p_testing$classe,pred.gbm)
   
   
   # Prediction using the provided testing set
   pred.test.gbm<-predict(model.gbm,testing_set)
   summary(pred.test.gbm)
   
   testing_pred<-cbind(testing_set,pred.test.gbm)
   View(testing_pred[,c(54,55)])
   
 
   
  