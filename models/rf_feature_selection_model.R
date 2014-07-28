library(MASS)

train.model <- function(trainingset, traininglabels)
{ 
  rf.model<-randomForest(x=as.matrix(trainingset), y=factor(traininglabels), n.trees=500)
  important.features <- names(sort(rf.model$importance[,1], decreasing=T))[1:15]
  trainingset <- trainingset[,important.features]
 
  logit.model <- cv.glmnet(as.matrix(trainingset), factor(traininglabels), nfolds=10, 
                           type.measure='deviance', family='binomial', alpha=0)
  return(list(logit.model=logit.model, important.features=important.features))
}

model.prediction <- function(model, testset)
{
  testset <- testset[,model$important.features]
  pred <- predict(model$logit.model, newx=as.matrix(testset), s='lambda.min', type='response')
  return(pred)
}