## 1. set the path
setwd("D:/git-ware/feature_selection_on_gene_mutation")
getwd()

## 2. get the data
source("getdata.R")
all_data = getdata()

## 3. get the gene mutation data
nci60GeneMut = all_data[,-1]

## 4. Feature selection: rfe: recursive feature elimination
library(caret)
require(glmnet)
set.seed(2016)
# funciton : random forest
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
ptm <- proc.time()
results <- rfe(nci60GeneMut, all_data[,1], sizes = c(1:55), rfeControl = control)
proc.time() - ptm
print(results)
str(predictors(results))
plot(results, type=c('g','o'))

## 5. save the data
m <- nci60GeneMut[,predictors(results)[1:55]]
all_data_rfe <- cbind(IC50 = all_data[,1], m)
write.csv(all_data_rfe, file = "selected_features/data_rfe.csv")
