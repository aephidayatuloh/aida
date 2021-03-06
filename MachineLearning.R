# #=======================================================================================
# #
# # File:        IntroToMachineLearning.R
# # Author:      Dave Langer
# # Description: This code illustrates the usage of the caret package for the An 
# #              Introduction to Machine Learning with R and Caret" Meetup dated 
# #              06/07/2017. More details on the Meetup are available at:
# #
# #                 https://www.meetup.com/data-science-dojo/events/239730653/
# #
# # NOTE - This file is provided "As-Is" and no warranty regardings its contents are
# #        offered nor implied. USE AT YOUR OWN RISK!
# #
# #=======================================================================================
# 
# #install.packages(c("e1071", "caret", "doSNOW", "ipred", "xgboost"))
# library(caret)
# library(doSNOW)
# 
# 
# 
# #=================================================================
# # Load Data
# #=================================================================
# # setwd("D:/Project/R")
# train <- read.csv("data/titanic-train.csv", stringsAsFactors = FALSE)
# # View(train)
# 
# 
# 
# 
# #=================================================================
# # Data Wrangling
# #=================================================================
# 
# # Replace missing embarked values with mode.
# table(train$Embarked)
# train$Embarked[train$Embarked == ""] <- "S"
# 
# 
# # Add a feature for tracking missing ages.
# summary(train$Age)
# train$MissingAge <- ifelse(is.na(train$Age),
#                            "Y", "N")
# 
# 
# # Add a feature for family size.
# train$FamilySize <- 1 + train$SibSp + train$Parch
# 
# 
# # Set up factors.
# train$Survived <- as.factor(train$Survived)
# train$Pclass <- as.factor(train$Pclass)
# train$Sex <- as.factor(train$Sex)
# train$Embarked <- as.factor(train$Embarked)
# train$MissingAge <- as.factor(train$MissingAge)
# 
# 
# # Subset data to features we wish to keep/use.
# features <- c("Survived", "Pclass", "Sex", "Age", "SibSp",
#               "Parch", "Fare", "Embarked", "MissingAge",
#               "FamilySize")
# train <- train[, features]
# str(train)
# 
# 
# 
# 
# #=================================================================
# # Impute Missing Ages
# #=================================================================
# 
# # Caret supports a number of mechanism for imputing (i.e., 
# # predicting) missing values. Leverage bagged decision trees
# # to impute missing values for the Age feature.
# 
# # First, transform all feature to dummy variables.
# dummy.vars <- dummyVars(~ ., data = train[, -1])
# train.dummy <- predict(dummy.vars, train[, -1])
# # View(train.dummy)
# 
# # Now, impute!
# pre.process <- preProcess(train.dummy, method = "bagImpute")
# imputed.data <- predict(pre.process, train.dummy)
# # View(imputed.data)
# 
# train$Age <- imputed.data[, 6]
# # View(train)
# 
# 
# 


preProc <- function(datapath, dev = TRUE)
{
  datas <- datapath
  
  #=================================================================
  # Data Wrangling
  #=================================================================
  
  # Replace missing embarked values with mode.
  table(datas$Embarked)
  datas$Embarked[datas$Embarked == ""] <- "S"
  
  
  # Add a feature for tracking missing ages.
  # summary(datas$Age)
  datas$MissingAge <- ifelse(is.na(datas$Age),
                            "Y", "N")
  
  
  # Add a feature for family size.
  datas$FamilySize <- 1 + datas$SibSp + datas$Parch
  
  
  # Set up factors.
  datas$Pclass <- as.factor(datas$Pclass)
  datas$Sex <- as.factor(datas$Sex)
  datas$Embarked <- as.factor(datas$Embarked)
  datas$MissingAge <- as.factor(datas$MissingAge)
  
  # Subset data to features we wish to keep/use.
  PassengerId <- datas$PassengerId
  if(dev)
  {
    datas$Survived <- as.factor(datas$Survived)
    datas.features <- c("Pclass", "Sex", "Age", "SibSp",
                        "Parch", "Fare", "Embarked", "MissingAge",
                        "FamilySize", "Survived")
  } else {
    datas.features <- c("Pclass", "Sex", "Age", "SibSp",
                        "Parch", "Fare", "Embarked", "MissingAge",
                        "FamilySize")
  }
  
  datas <- datas[, datas.features]
  # str(datas)
  
  
  #=================================================================
  # Impute Missing Ages
  #=================================================================
  
  # Caret supports a number of mechanism for imputing (i.e., 
  # predicting) missing values. Leverage bagged decision trees
  # to impute missing values for the Age feature.
  
  # First, transform all feature to dummy variables.
  dummy.vars <- dummyVars(~ ., data = datas)
  datas.dummy <- predict(dummy.vars, datas)
  # View(train.dummy)
  
  # Now, impute!
  pre.process <- preProcess(datas.dummy, method = "bagImpute")
  imputed.data <- predict(pre.process, datas.dummy)
  # View(imputed.data)
  
  datas$Age <- imputed.data[, 6]
  datas$Fare[is.na(datas$Fare)] <- median(datas$Fare, na.rm = TRUE)
  # View(train)
  datas <- cbind(PassengerId, datas)
  return(datas)
}

modelDev <- function(datas)
{
  #=================================================================
  # Split Data
  #=================================================================

  # Use caret to create a 70/30% split of the training data,
  # keeping the proportions of the Survived class label the
  # same across splits.
  # set.seed(54321)
  indexes <- createDataPartition(datas$Survived,
                                 times = 1,
                                 p = 0.7,
                                 list = FALSE)
  titanic.train <- datas[indexes,]
  titanic.test <- datas[-indexes,]


  # Examine the proportions of the Survived class lable across
  # the datasets.
  prop.table(table(datas$Survived))
  prop.table(table(titanic.train$Survived))
  prop.table(table(titanic.test$Survived))




  #=================================================================
  # Train Model
  #=================================================================

  # Set up caret to perform 10-fold cross validation repeated 3
  # times and to use a grid search for optimal model hyperparamter
  # values.
  train.control <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 3,
                                search = "grid")


  # Leverage a grid search of hyperparameters for xgboost. See
  # the following presentation for more information:
  # https://www.slideshare.net/odsc/owen-zhangopen-sourcetoolsanddscompetitions1
  # tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
  #                          nrounds = c(50, 75, 100),
  #                          max_depth = 6:8,
  #                          min_child_weight = c(2.0, 2.25, 2.5),
  #                          colsample_bytree = c(0.3, 0.4, 0.5),
  #                          gamma = 0:2,
  #                          subsample = 1:2)
  # # grid search of hyperparameters for random forest
  # tune.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
  # grid search of hyperparameters for random forest
  tune.grid <- expand.grid(mtry = c(1:15))
  # View(tune.grid)


  # Use the doSNOW package to enable caret to train in parallel.
  # While there are many package options in this space, doSNOW
  # has the advantage of working on both Windows and Mac OS X.
  #
  # Create a socket cluster using 10 processes.
  #
  # NOTE - Tune this number based on the number of cores/threads
  # available on your machine!!!
  #
  cl <- makeCluster(10, type = "SOCK")

  # Register cluster so that caret will know to train in parallel.
  registerDoSNOW(cl)

  # Train the xgboost model using 10-fold CV repeated 3 times
  # and a hyperparameter grid search to train the optimal model.
  caret.cv <<- train(Survived ~ .,
                    data = titanic.train,
                    method = "rf",
                    tuneGrid = tune.grid,
                    trControl = train.control)
  stopCluster(cl)


  # Examine caret's processing results
  list(caret.cv, titanic.test)
}

confMat <- function(models)
{
  print(models)
  # Make predictions on the test set using a xgboost model
  # trained on all 625 rows of the training set using the
  # found optimal hyperparameter values.
  preds <- predict(models$caret.cv, models$titanic.test)


  # Use caret's confusionMatrix() function to estimate the
  # effectiveness of this model on unseen, new data.
  confM <- confusionMatrix(preds, titanic.test$Survived)
  return(confM)
}

# To predict test data
predTestFun <- function(datapath)
  {

  caret.cv.pred <- readRDS("data/caret-cv.rds")
  
  test.preds <- predict(caret.cv.pred, datapath)
  table(test.preds)
  test.preds <- data.frame("PassengerId" = datapath$PassengerId, "Survived" = test.preds)
  names(test.preds) <- c("PassengerId", "Survived")
  write.csv(test.preds, "data/titanic_rf.csv", row.names = F)
}