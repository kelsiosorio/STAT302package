#' Random Forest Cross-Validation
#'
#' This function predicts output body_mass_g using covariates bill_length_mm, bill_depth_mm, and flipper_length_mm.
#'
#' @param k Number of folds
#' @keywords prediction
#'
#' @return A numeric with the cross validation error
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(7)
#'
#' @export
my_rf_cv <- function(k) {
  # Clean data
  clean_penguins <- stats::na.omit(STAT302package::my_penguins %>% dplyr::select(bill_length_mm,
                                                                              bill_depth_mm,
                                                                              flipper_length_mm,
                                                                              body_mass_g))
  # split data into k parts, randomly
  fold <- sample(rep(1:k, length = nrow(clean_penguins)))
  # binding data together
  data <- cbind(clean_penguins, fold)
  # initializing data frame with errors
  mse_df <- data.frame()
  for (i in 1:k) {
    # splitting into training and testing data
    data_train <- data %>% dplyr::filter(fold != i)
    data_test <- data %>% dplyr::filter(fold == i)
    # running a random forest model
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                                        data = data_train,
                                        ntree = 100)
    # finding predictions
    predictions <- stats::predict(model, data_test[, -4])
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

utils::globalVariables(c("bill_length_mm", "bill_depth_mm", "body_mass_g", "flipper_length_mm"))
