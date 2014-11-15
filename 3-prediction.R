library(randomForest)
library(pROC)


######################################################################
#####
##### We want to use the date before 2008 to predict the outcomes in 2008
#####

### Create training data before 2008
train.data <- summarized[summarized$Year < 2008, ]

### Remaining data from 2008
summarized.2008 <- summarized[summarized$Year == 2008, ]

###### Create test and hold out set
### Test set: 25% of 2008 data
### Hold-out set: 75% of 2008 data

### Setting the seed
set.seed(75)
test.data <- summarized.2008[sample(nrow(summarized.2008), size=round(nrow(summarized.2008) * 0.25)), ]
hold.out <- setdiff(summarized.2008, test.data)

### Remove previous 2008 data
rm(summarized.2008)

### Remove year data
train.data$Year <- NULL
test.data$Year <- NULL
hold.out$Year <- NULL

######################################################################
###
### Train model (Random Forest)
###
rf <- randomForest(formula = Class ~ ., data = train.data, importance=TRUE, ntree=500)

### Save importance of random forest
library(caret)
vaImp.rf <- varImp(rf, verbose = FALSE)

### Prediction on test data
result.predicted.test <- predict(rf, test.data, type="prob")
### Get ROC result
result.roc <-  roc(test.data$Class, result.predicted.test[, 1])

###
### Result of model
###
result.roc
### Depending on the seed the result (area under the curve) should be around 0.93.

###
### Plot result
###
plot(result.roc.model1, print.thres="best", print.thres.best.method="closest.topleft")