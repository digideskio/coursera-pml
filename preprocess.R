#str(pml.training)
#table(pml.training$classe)
#prop.table(table(pml.training$new_window))
#summary(pml.training)

#qplot(yaw_belt, total_accel_belt, data=pml.training, colour=classe)
#qplot(yaw_belt, data=pml.training, colour=classe, geom="density")

#qq <- qplot(yaw_belt, total_accel_belt, data=pml.training, colour=classe)
#qq + geom_smooth(method='lm', formula=y~x)

#featurePlot(x=pml.training[,c("yaw_belt", "total_accel_belt")],y=pml.training$classe, plot="pairs")


y <- c("classe")
# discard features that don't seem valuable
xs <- setdiff(colnames(pml.training), c("new_window", "num_window", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "classe"))

filterRows <- function(ds) {
  ds[ds$new_window == "no",];
}

train <- filterRows(pml.training);
test <- filterRows(pml.testing);


# drop columns that are mostly empty (NA, empty string or #DIV/0!)
mostlyEmpty <- sapply(xs, function(x) sum(is.na(train[, x]) | train[, x] == "" | train[, x] == "#DIV/0!") > 0.75 * nrow(train))
xs <- xs[!mostlyEmpty]

train <- train[,c(xs, y)]
test <- test[,c(xs)]

