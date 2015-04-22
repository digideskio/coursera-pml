setwd("~/rspective/ML/kaggle_old/pml")

library(ggplot2);
library(caret);
library(Hmisc);
library(rattle);
library(rpart);

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

set.seed(3433)

pml.training <- read.csv("pml-training.csv")
pml.testing <- read.csv("pml-testing.csv")