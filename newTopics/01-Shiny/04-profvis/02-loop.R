profvis({
  data <- data.frame(value = runif(3e4))
  
  data$sum[1] <- data$value[1]
  for (i in seq(2, nrow(data))) {
    data$sum[i] <- data$sum[i-1] + data$value[i]
  }
}, height = "350px")