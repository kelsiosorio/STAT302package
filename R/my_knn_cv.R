#' k-nearest neighbors cross validation
#'
#' This function predicts output class using covariates and finds CV misclassification error.
#'
#' @param train Inpuut data frame of covariates.
#' @param cl True class value of your training data/covariates.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return  A list with the following elements:
#'   class: a vector of the predicted class for all observations
#'   cv_error: a numeric with the cross-validation misclassification error.
#'
#' @examples
#' train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#' cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#' my_knn_cv(train, cl, 4, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # split data into k_cv parts, randomly
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  # combining all data
  data <- data.frame("train" = train, "cl" = cl, "split" = fold)
  # initializind miss calc rate df
  m_rate_df <- data.frame()
  for (i in 1:k_cv) {
    # splitting into training and testing data and pulling cl out
    data_train <- data %>% dplyr::filter(split != i)
    train_clean <- data_train %>% dplyr::select(-split, -cl)
    cl <- data_train %>% dplyr::pull(cl)
    data_test <- data %>% dplyr::filter(split == i)
    test_clean <- data_test %>% dplyr::select(-split, -cl)
    # finding k nearest neighbors
    p_class <- class::knn(train = train_clean,
                      test = test_clean,
                      cl = cl,
                      k = k_nn)
    # calculating proportion of observations that were classified incorrectly
    miss_rate <- mean(ifelse(data_test$cl == p_class, 0, 1))
    # adding that number to table
    m_rate_df <- rbind(m_rate_df, i = miss_rate)
  }
  # getting rid of label for reading purposes
  colnames(m_rate_df) <- NULL
  # storing predicted class using all data
  class <- as.character(class::knn(train = train,
                            test = train,
                            cl = data %>% dplyr::pull(cl),
                            k_nn))
  # finding average of all miscalculation errors
  cv_error <- colMeans(m_rate_df)
  # creaing list to output
  my_list <- list("class" = class,
                  "cv_err" = cv_error)
  return(my_list)
}
