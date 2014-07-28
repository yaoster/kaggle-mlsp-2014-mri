train.model <- function(trainingset, traininglabels)
{
  fnc.data <- trainingset[,1:378]
  sbm.data <- trainingset[,379:dim(trainingset)[2]]
  
  # FNC elastic net model
  fnc.model <- cv.glmnet(as.matrix(fnc.data), factor(traininglabels), nfolds=10, 
                         type.measure='deviance', family='binomial', alpha=.3)
  
  # SBM elastic net model
  sbm.model <- cv.glmnet(as.matrix(sbm.data), factor(traininglabels), nfolds=10, 
                         type.measure='deviance', family='binomial', alpha=.075)
  
  # Elastic net model
  all.model <- cv.glmnet(as.matrix(cbind(fnc.data, sbm.data)), factor(traininglabels), nfolds=10, 
                         type.measure='deviance', family='binomial', alpha=.95)
  
  # RF feature screen + ridge
  rf.trainingset <- cbind(fnc.data, sbm.data)
  rf.model<-randomForest(x=as.matrix(rf.trainingset), y=factor(traininglabels), n.trees=500)
  important.features <- names(sort(rf.model$importance[,1], decreasing=T))[1:15]
  rf.trainingset <- rf.trainingset[,important.features]
  rf.logit.model <- cv.glmnet(as.matrix(rf.trainingset), factor(traininglabels), nfolds=10, 
                              type.measure='deviance', family='binomial', alpha=0)
  
  # Sparse PCA + elastic net model
  out <- SPC(as.matrix(trainingset), sumabsv=10, K=20)
  pca.trainingset <- as.matrix(trainingset) %*% out$v
  pca.model <- cv.glmnet(pca.trainingset, factor(traininglabels), nfolds=10, 
                         type.measure='deviance', family='binomial', alpha=1)
  
  return(list(fnc.model=fnc.model, 
              sbm.model=sbm.model, 
              all.model=all.model, 
              rf.logit.model=rf.logit.model, 
              important.features=important.features, 
              pca.model=pca.model,
              out=out))
}

model.prediction <- function(model, testset)
{
  fnc.data <- testset[,1:378]
  sbm.data <- testset[,379:dim(testset)[2]]
  
  rf.testset <- testset[,model$important.features]
  rf.pred <- c(predict(model$rf.logit.model, newx=as.matrix(rf.testset), s='lambda.min', type='response'))
  fnc.pred <- c(predict(model$fnc.model, newx=as.matrix(fnc.data), s='lambda.min', type='response'))
  sbm.pred <- c(predict(model$sbm.model, newx=as.matrix(sbm.data), s='lambda.min', type='response'))
  all.pred <- c(predict(model$all.model, newx=as.matrix(testset), s='lambda.min', type='response'))
  pca.testset <- as.matrix(testset) %*% model$out$v
  pca.pred <- predict(model$pca.model, newx=pca.testset, s='lambda.min', type='response')

  ensemble.pred <- .2*all.pred+.2*fnc.pred+.2*sbm.pred+.2*rf.pred+.2*pca.pred
  return(ensemble.pred)
}