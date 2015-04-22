target <- "Survived";
#features <- c("Fare", "Pclass", "Title", "AgeFill", "Sex", "Level", "Embarked", "Family"); 
features <- c("Fare", "Class_1", "Class_2", "Class_3", "Sex", "AgeFill" ); 

inTrain <- createDataPartition(train[,target], p = split, list=FALSE);
training <- train[inTrain, c(target, features)];
testing <- train[-inTrain, c(target, features)];

# BASIC
model <- train(Survived ~ ., data=training, method = "rf", preProcess = "pca");
#model <- train(Survived ~ ., data=training, method = "rf", preProcess = "pca", trControl = trainControl(method="cv"), numbers=3);

# RPART
# model <- rpart(Survived ~ ., data=training);
#fancyRpartPlot(model);

# COMBINED MODELS
#model1 <- train(Survived ~ ., data=training, method = "glm", preProcess = "pca");
#predictions1 <- predict(model1, testing);
#model2 <- train(Survived ~ ., data=training, method = "rf", preProcess = "pca", trControl = trainControl(method="cv"), numbers=3);
#predictions2 <- predict(model2, testing);
#model3 <- rpart(Survived ~ ., data=training);
#predictions3 <- predict(model3, testing);
#modelCombined <- data.frame(predictions1, predictions2, Survived=testing$Survived);
#model <- train(Survived ~ ., data=modelCombined, method="gam");

predictions_raw <- predict(model, testing);
predictions <- ifelse(predictions_raw >= prediction_thresh, 1, 0);
predictions <- as.integer(predictions);
cm <- confusionMatrix(predictions, testing[, target]);

testing_fp <- testing[(predictions == 1 & testing[,target] == 0),];
testing_fn <- testing[(predictions == 0 & testing[,target] == 1),];
testing_tp <- testing[(predictions == 1 & testing[,target] == 1),];
testing_tn <- testing[(predictions == 0 & testing[,target] == 0),];