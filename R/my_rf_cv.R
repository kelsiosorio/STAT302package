my_rf_cv <- function(k) {
  # split data into k parts, randomly
  fold <- sample(rep(1:k, length = nrow(data_clean2)))
  # binding data together
  data <- cbind(data_clean2, fold)
  # initializing data frame with errors
  mse_df <- data.frame()
  for (i in 1:k) {
    # splitting into training and testing data
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    # running a random forest model
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                          data = data_train,
                          ntree = 100)
    # finding predictions
    predictions <- predict(model, data_test[, -4])
    # calculating MSE
    mse <- sum((predictions - data_test$body_mass_g)^2) / nrow(data_test)
    # adding that number to table
    mse_df <- rbind(mse_df, i = mse)
  }
  # adding label for readability
  colnames(mse_df) <- c("CV_MSE")
  # finding average of MSE
  CV_MSE <- colMeans(mse_df)
  return(CV_MSE)
}
