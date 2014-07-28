library(glmnet)
library(verification)
source('ensemble_model.R')

# load data
fnc.data <- read.table('data/train_FNC.csv', sep=',', header=T)
sbm.data <- read.table('data/train_SBM.csv', sep=',', header=T)
all.data <- cbind(fnc.data[,2:dim(fnc.data)[2]], sbm.data[,2:dim(sbm.data)[2]])
labels <- read.table('data/train_labels.csv', sep=',', header=T)$Class

# perform leave-one-out cross validation and print AUC
k <- 86
id <- 1:86 #sample(1:k,nrow(all.data),replace=TRUE)
list <- 1:k
prediction <- c()
for (i in 1:k) {
  print(i)
  trainingset <- subset(all.data, id %in% list[-i])
  traininglabels <- subset(labels, id %in% list[-i])
  testset <- subset(all.data, id %in% i)
  testlabels <- subset(labels, id %in% i)
  
  model <- train.model(trainingset, traininglabels)
  temp <- model.prediction(model, testset)
  prediction <- c(prediction, temp)
}
print(roc.area(labels, prediction)$A)
