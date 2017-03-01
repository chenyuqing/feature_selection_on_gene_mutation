## 1. set the path
setwd("D:/git-ware/feature_selection_on_gene_mutation")
getwd()

## 2. load the library
library(cvTools)  # Cross-validation
library(randomForest)  # random forest
library(e1071) # svm
library(gbm) # gradient-boosting regression

## 3. read the data
# all_data <- read.csv(file.path("selected_features", "data_rfe.csv"), row.names = 1)
all_data <- read.csv(file.path("selected_features", "data_mrmr.csv"), row.names = 1)
# all_data <- read.csv(file.path("selected_features", "data_pca.csv"), row.names = 1)

## 4. cross validation
p_data <- all_data
k = 10
folds <- cvFolds(NROW(p_data), K = k)
p_data$holdoutpred <- rep(0,nrow(p_data))

ptm <- proc.time()
for(i in 1:k){
  # Set the training set
  train <- p_data[folds$subsets[folds$which != i], ] 
  # Set the validation set
  validation <- p_data[folds$subsets[folds$which == i], ] 
  
  # 1. Get your new random forest model (just fit on the train data)
  # newbm <- randomForest(IC50~., data = train, ntree = 500) 
  # 2. Get your new svm model
  # newbm <- svm(IC50~., data = train)
  # 3. gradient-boosting regression
  newbm <- gbm(IC50~., data = train, n.trees = 10, distribution = "gaussian")
  
  # Get the predicitons for the validation set 
  # (from the model just fit on the train data)
  # newpred <- predict(newbm, newdata = validation) 
  newpred <- predict(newbm, newdata = validation, n.trees = 10) 
  
  #Put the hold out prediction in the data set for later use
  p_data[folds$subsets[folds$which == i], ]$holdoutpred <- newpred 
}
proc.time() - ptm

## 5. Evaluate the model
source("rmse.R")
source("rsqure.R")

## 6. get the metrics
# rmse
rmse <- rmse(p_data$holdoutpred-p_data$IC50)
rmse
# R square
rq <- rsqure(p_data$IC50, p_data$holdoutpred)
rq
