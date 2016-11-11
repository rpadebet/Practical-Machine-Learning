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
              "cvtd_timestamp","new_window")
   training_set<-training_set[,!(names(training_set) %in% remCols)]

 # Removing same cols from testing set

   testing_set<-subset(testing,select = c(names(training_set[,-c(54)]),"problem_id"))
   
 # Creating own training and testing sets
   set.seed(1234)
   inTrain = createDataPartition(training_set$classe, p = 3/4)[[1]]
   p_training = training_set[ inTrain,]
   p_testing = training_set[-inTrain,]
   
 # Visually exploring data
   featurePlot(x = p_training[, 1:16], 
               y = p_training$classe,
               plot = "density", 
               scales = list(x = list(relation="free"), 
                             y = list(relation="free")), 
               adjust = 1.5, 
               pch = "|", 
               layout = c(4, 4), 
               auto.key = list(columns = 5))

# Modeling
   set.seed(1234)
   library(caret)
   library(doParallel)
   
   trainctrl<-trainControl(method="repeatedcv"
                           ,number=4,repeats = 3
                           ,verboseIter = TRUE
                           ,allowParallel = TRUE)
   
   preprocess<-c("center", "scale")
   myGrid<-expand.grid(mtry=seq(1,50,5))
   cl <- makeCluster(detectCores()-1)

   
   # Random Forest Model
   registerDoParallel(cl)
   system.time(
   model.fit<-train(form = classe ~ .,
                   model = "rf",
                   data = p_training,
                   metric="Accuracy",
                   preProcess = preprocess,
                   trControl = trainctrl,
                   tuneGrid = myGrid,
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
   View(testing_pred[,c(54,55)])
   
 
   
  