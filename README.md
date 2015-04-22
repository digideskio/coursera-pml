# coursera-pml
Practical Machine Learning with coursera

* Discard features that don't seem to be valuable.
* Remove rows with new_window=yes. They seem to be some kind of summary rows, cluttering the dataset.
* Drop columns that are more than 75% empty (NA, empty string or #DIV/0!), as they most probably won't add much value.