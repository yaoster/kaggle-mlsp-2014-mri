library(MASS)
library(PMA)

train.model <- function(trainingset, traininglabels)
{ 
  out <- SPC(as.matrix(trainingset), sumabsv=10, K=20)
  trainingset <- as.matrix(trainingset) %*% out$v
  glmnet.model <- cv.glmnet(trainingset, factor(traininglabels), nfolds=10, 
                            type.measure='deviance', family='binomial', alpha=1)
  return(list(glmnet.model=glmnet.model, out=out))
}

model.prediction <- function(model, testset)
{
  testset <- as.matrix(testset) %*% model$out$v
  pred <- predict(model$glmnet.model, newx=testset, s='lambda.min', type='response')
  return(pred)
}