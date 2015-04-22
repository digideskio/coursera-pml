#str(pml.training)
#table(pml.training$classe)
#prop.table(table(pml.training$new_window))
#summary(pml.training)

#qplot(yaw_belt, total_accel_belt, data=pml.training, colour=classe)
#qplot(yaw_belt, data=pml.training, colour=classe, geom="density")

#qq <- qplot(yaw_belt, total_accel_belt, data=pml.training, colour=classe)
#qq + geom_smooth(method='lm', formula=y~x)

#featurePlot(x=pml.training[,c("yaw_belt", "total_accel_belt")],y=pml.training$classe, plot="pairs")


filterNW <- function(ds) {
  ds[ds$new_window == "no",];
}

train <- filterNW(pml.training);
test <- filterNW(pml.testing);

process <- function(ds) {
  # general data analysis tools
  #summary(ds)
  #str(ds)
  #table(ds$Survived)
  #qplot(Sex, Age, data=ds, color=Survived)
  
  #mean(ds$Age)
  #sd(ds$Age)
  #hist(ds$Age)
  
  # user_name - factor w/ 6
  
  #Class
  ds[, "Class_1"] <- ifelse(ds$Pclass == 1, 1, 0);
  ds[, "Class_2"] <- ifelse(ds$Pclass == 2, 1, 0);
  ds[, "Class_3"] <- ifelse(ds$Pclass == 3, 1, 0);
  
  #Name
  # Assumption: 
  # 'Capt': 2, 'Don':2, 'Major':2, 'Sir':2,
  # 'the Countess':2, 'Lady':2, 'Jonkheer':2, 'Dona': 2,
  # 'Dr':2, 'Col':2, 'Rev':2,
  # 'Mrs':1, 'Ms':1, 'Mr':1, 'Mme':1, 'Mlle':1, 'Miss':1, 'Master':1
  ds[,"Title"] <- ifelse(grepl("Capt\\.|Don\\.|Major\\.|Sir\\.|the Countess\\.|Lady\\.|Jonkheer\\.|Don\\.|Dr\\.|Col\\.|Rev\\.", ds$Name), 2, 1);
  ds[, "RegularTitle"] <- ifelse(ds$Title == 1, 1, 0);
  ds[, "NobleTitle"] <- ifelse(ds$Title == 2, 1, 0);
  
  # x <- train[,c("Title", "Name")] # code for checking Title grep
  # x[ order(x[,"Title"], x[,"Name"]), ]
  
  #Sex
  ds[, "IsMale"] <- ifelse(ds$Sex == "male", 1, 0);
  ds[, "IsFemale"] <- ifelse(ds$Sex == "female", 1, 0)
  
  #Age
  ds$AgeFill <- with(ds, impute(Age, median))
  
  #Ticket
  
  # Parch, SibSp
  ds[, "Family"] <- ds$Parch + ds$SibSp + 1;
  # cbind(train$Family, train$SibSp, train$Parch, 1) # checks if it sums up correctly
  
  #Fare
  ds$Fare <- with(ds, impute(Fare, median))
  ds$Fare_pp <- ds$Fare/ds$Family;
  
  ds$Pclass_pred <- ifelse(ds$Fare_pp > 50, 1, NA)
  ds$Pclass_pred <- ifelse(ds$Fare_pp <= 50, 2, ds$Pclass_pred)
  ds$Pclass_pred <- ifelse(ds$Fare_pp <= 15, 3, ds$Pclass_pred)
  
  #Cabin
  ds[, "Level"] <- NA;
  ds[grepl("G", ds$Cabin), "Level"] <- 7;
  ds[grepl("F", ds$Cabin), "Level"] <- 6;
  ds[grepl("E", ds$Cabin), "Level"] <- 5;
  ds[grepl("D", ds$Cabin), "Level"] <- 4;
  ds[grepl("C", ds$Cabin), "Level"] <- 3;
  ds[grepl("B", ds$Cabin), "Level"] <- 2;
  ds[grepl("A", ds$Cabin), "Level"] <- 1;
  ds$Level <- with(ds, impute(Level, median))
  
  #Embarked
  ds[, "Embarked_S"] <- ifelse(ds$Embarked == "S" | ds$Embarked == "", 1, 0);
  ds[, "Embarked_C"] <- ifelse(ds$Embarked == "C", 1, 0);
  ds[, "Embarked_Q"] <- ifelse(ds$Embarked == "Q", 1, 0);
  
  ds;
}

#train <- process(train);
#test <- process(test);