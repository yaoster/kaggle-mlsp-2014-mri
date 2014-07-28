source('ensemble_model.R')

print('Loading test data')
fnc.data <- read.table('data/test_FNC.csv', sep=',', header=T)
sbm.data <- read.table('data/test_SBM.csv', sep=',', header=T)
all.data <- cbind(fnc.data, sbm.data)

print('Loading training data')
training.fnc.data <- read.table('data/train_FNC.csv', sep=',', header=T)
training.sbm.data <- read.table('data/train_SBM.csv', sep=',', header=T)
training.all.data <- cbind(training.fnc.data, training.sbm.data)
labels <- read.table('data/train_labels.csv', sep=',', header=T)$Class

print('Training model')
model <- train.model(training.all.data, labels)
pred <- model.prediction(model, all.data)
results <- data.frame(Id=sbm.data$Id, Probability=pred)
write.table(results, file='results.csv', quote=F, sep=',', row.names=F)
