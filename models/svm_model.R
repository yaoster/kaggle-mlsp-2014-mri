library(e1071)

train.model <- function(trainingset, traininglabels, cost)
{ 
  model <- svm(x=as.matrix(trainingset), y=factor(traininglabels), cost=cost, probability=T, kernel='linear') #.185
  return(list(model=model, range=range(model$decision.values)))
}

model.prediction <- function(model, testset)
{
  pred <- c(attributes(predict(model$model, newdata=as.matrix(testset), probability=T, decision.values=T))$decision.values)
  pred[pred > model$range[2]] <- model$range[2]
  pred[pred < model$range[1]] <- model$range[1]
  pred <- (pred - model$range[1])/(abs(model$range[2]) + abs(model$range[1]))
  return(pred)
}