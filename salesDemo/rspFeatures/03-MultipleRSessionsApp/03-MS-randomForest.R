########################################
### Predictive analalytics           ###
### Comparison of alternative models ###
### Data: mtcars                     ###
########################################

### Load libraries
library(randomForest)
library(gam)
library(e1071)

### Build models
model1 <- randomForest(mpg ~ ., mtcars)
model2 <- gam(mpg ~ ., mtcars, family = 'gaussian')
model3 <- svm(mpg ~ ., mtcars)

### Make predictions
pred <- data.frame(
  actual = mtcars$mpg,
  randomForest = predict(model1),
  gam = predict(model2),
  svm = predict(model3)
)

### Compare MSE
sapply(2:4, function(x) mean((pred[,1]-pred[,x])^2))

### Write predictions to file
write.csv(pred, './rspFeatures/03-MultipleRSessionsApp/pred_alt.csv')
