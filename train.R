inTrain  <- createDataPartition(train[, y], p = 0.75, list=FALSE);
training <- train[inTrain, c(y, xs)];
testing  <- train[-inTrain, c(y, xs)];

# BASIC
model <- train(classe ~ ., data = training, method = "rf", preProcess = "pca");

# RPART
#model <- rpart(classe ~ ., data=training);
#fancyRpartPlot(model);

predictions <- predict(model, testing);
cm <- confusionMatrix(predictions, testing[, y]);