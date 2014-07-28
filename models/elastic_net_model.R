train.model <- function(trainingset, traininglabels)
{
  fnc.data <- trainingset[,1:378]
  sbm.data <- trainingset[,379:dim(trainingset)[2]]
  
  fnc.model <- cv.glmnet(as.matrix(fnc.data), factor(traininglabels), nfolds=10, 
                         type.measure='deviance', family='binomial', alpha=.3)
  sbm.model <- cv.glmnet(as.matrix(sbm.data), factor(traininglabels), nfolds=10, 
                         type.measure='deviance', family='binomial', alpha=.075)

  return(list(fnc.model=fnc.model,sbm.model=sbm.model))
}

model.prediction <- function(model, testset)
{
  fnc.data <- testset[,1:378]
  sbm.data <- testset[,379:dim(testset)[2]]
  
  fnc.pred <- predict(model$fnc.model, newx=as.matrix(fnc.data), s='lambda.min', type='response')
  sbm.pred <- predict(model$sbm.model, newx=as.matrix(sbm.data), s='lambda.min', type='response')

  return(.5*fnc.pred + .5*sbm.pred)
}
