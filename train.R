inTrain  <- createDataPartition(train[, y], p = 0.7, list=FALSE);
training <- train[inTrain, c(y, xs)];
testing  <- train[-inTrain, c(y, xs)];

model <- train(classe ~ ., data = training, method = "rf", trControl = trainControl(method="cv"), numbers=3);
# model <- train(classe ~ ., data = training, method = "pam", preProcess = "pca");

# RPART
# model <- rpart(classe ~ ., data=training);
# fancyRpartPlot(model);


predictions <- predict(model, testing);
cm <- confusionMatrix(predictions, testing[, y]);
cm

# variable importance
plot(varImp(model, scale = FALSE))

save(model, file="model_rf_cv_3_0.7.rda")


# submitting answers
pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

answers <- predict(model, test);
pml_write_files(answers)

# rf + trControl=cv + numbers=3
#0.2  => 1m,  0.97 accuracy, prediction 0.97
#0.7  => 15m, 0.99 accuracy, prediction 0.99

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