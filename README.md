# coursera-pml
Practical Machine Learning with coursera

## Data analysis

We begin with analysing the data manually. The training dataset contains 19.622 observations of 160 variables. The test dataset consists of 20 rows.

The output parameter is named `classe` and is a factor with 5 values.

```R
> str(train$classe)
 Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
> summary(train$classe)
   A    B    C    D    E 
5471 3718 3352 3147 3528 
```

Just looking at the data, there seems to be quite a lot of empty cells and NAs. Good - we'll be able to trim down the number of colums before we actually feed the data to the training algorithms.

Another interesting observation is that every ~24 rows there seems to be some kind of a summary row copy-pasted from excel - it has string values of `#DIV/0!`, and in general it has very different data than the _regular_ rows - it's marked with `new_window=yes`. This makes me think that it would be wise to divide the data into two parts based on the `new_window` column values. Furthermore we could try to build two separate models for the two parts. But for now, since it's only ~400 rows out of 19k (unnecessary clutter?), we'll skip these rows in the processing.


```R
table(pml.training$new_window)

   no   yes 
19216   406 
```

Column `X` is just an ID, so it won't be used as a feature.

Column `user_name` is a factor with 6 values. The assignment is about analysing whether a physical excercise is performed correctly or incorrectly, and classifying the type of errors. Although we could use this feature as the test set has exactly the same factors for this column, the algorithm shouldn't be dependent on the `user_name` of the person doing excercises, should be able to predict independently of who does the excercises.

```R
table(pml.training$user_name)

  adelmo carlitos  charles   eurico   jeremy    pedro 
    3892     3112     3536     3070     3402     2610 
```

There are three timestamp columns, that we might consider. Idea - figure out for how long the person has been excercising already - maybe there's a correlation: fatigue vs error type.

`num_window` seems to be some metadata related to the capturing devices. Discarding.

Now there seem to be a lot of columns related to the sensor measurements - still there's so many columns it's hard to go thru them all an filter manually. The general intuition is that we should be using features related to `arm`s, `belt`s, `dumbbell`s and `forearm`s. The rest of columns look promising for training models and running predictions.

## Data preprocessing & Feature selection

We begin with the low-hanging fruits. Let's follow the intuition, let's run some naive preprocessing to clean the data, and see what kind of results we get out of it.

Regarding the cleaning - I decided to follow the approach of discarding all columns that have more than 75% empty cells (empty means `''`, `NA` or `#DIV/0!`). Most probably, these columns won't add much value to the final output.

```R
y <- c("classe")
# discard features that don't seem valuable
xs <- setdiff(colnames(pml.training), c("X", "user_name", "new_window", "num_window", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "classe"))

# remove the summary rows
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
```

At this point, the numbers of potential features fell from 160 to 52. Seems like we just discarded a lot of crap :).

```R
xs
 [1] "roll_belt"            "pitch_belt"           "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
 [6] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"         "accel_belt_y"         "accel_belt_z"        
[11] "magnet_belt_x"        "magnet_belt_y"        "magnet_belt_z"        "roll_arm"             "pitch_arm"           
[16] "yaw_arm"              "total_accel_arm"      "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
[21] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"          "magnet_arm_x"         "magnet_arm_y"        
[26] "magnet_arm_z"         "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"         "total_accel_dumbbell"
[31] "gyros_dumbbell_x"     "gyros_dumbbell_y"     "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
[36] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"    "magnet_dumbbell_z"    "roll_forearm"        
[41] "pitch_forearm"        "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"      "gyros_forearm_y"     
[46] "gyros_forearm_z"      "accel_forearm_x"      "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
[51] "magnet_forearm_y"     "magnet_forearm_z"    
```

## Training model

Since it's a classification problem, not all algorithms will fit.

## Conclusion

