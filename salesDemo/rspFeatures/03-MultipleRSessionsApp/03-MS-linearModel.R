########################################
### Predictive analalytics           ###
### Comparison of linear models      ###
### Data: mtcars                     ###
########################################

### Load libraries
library(stats)
library(MASS)
library(glmnet)

### Build models
model1 <- lm(mpg ~ ., mtcars)
model2 <- stepAIC(model1, trace = 0)
model3 <- cv.glmnet(as.matrix(mtcars[, -1]), mtcars[, 1])

### Make predictions
pred <- data.frame(
  actual = mtcars$mpg,
  linear = predict(model1),
  stepwise = predict(model2),
  lasso = predict(model3, as.matrix(mtcars[, -1]), s = 'lambda.min')[,1]
)

### Compare MSE
sapply(2:4, function(x) mean((pred[,1]-pred[,x])^2))

### Write predictions to file
write.csv(pred, './rspFeatures/03-MultipleRSessionsApp/pred_lm.csv')
