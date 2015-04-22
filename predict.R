predictions_final_raw <- predict(model, test[, features]);
predictions_final <- ifelse(predictions_final_raw >= prediction_thresh, 1, 0);
predictions_final <- as.integer(predictions_final);

PassengerId <- test$PassengerId;
Survived <- predictions_final;
results <- cbind(PassengerId, Survived);
write.csv(results, file = "results.csv", row.names=FALSE,)