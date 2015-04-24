inTrain  <- createDataPartition(train[, y], p = 0.2, list=FALSE);
training <- train[inTrain, c(y, xs)];
testing  <- train[-inTrain, c(y, xs)];

model <- train(classe ~ ., data = training, method = "rf", preProcess = "pca", trControl = trainControl(method="cv"), numbers=3);
# model <- train(classe ~ ., data = training, method = "pam", preProcess = "pca");

# RPART
model <- rpart(classe ~ ., data=training);
fancyRpartPlot(model);

predictions <- predict(model, testing);
cm <- confusionMatrix(predictions, testing[, y]);

# rf + pca
#0.05 => 30s, 0.73 accuracy
#0.1  => 1m,  0.81 accuracy, prediction 0.86

# rf + pca + trainControl=cv + numbers=3
#0.1  => 30s,  0.85 accuracy, prediction 0.86
#0.2  => 1m,   0.91 accuracy, prediction 0.92
#0.3  => 1.5m, 0.93 accuracy, prediction 0.94
#0.7  => 15m,  0.97 accuracy, prediction 0.98

# lda2 + pca
# 0.1 => 3s, 0.5 accuracy, prediction 0.5

# pam + pca
# 0.1 => 0.4 accuracy