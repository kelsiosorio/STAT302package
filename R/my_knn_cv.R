my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # split data into k_cv parts, randomly
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  # combining all data
  data <- data.frame("train" = train, "cl" = cl, "split" = fold)
  # initializind miss calc rate df and prediction list
  m_rate_df <- data.frame()
  predictions <- list()
  for (i in 1:k_cv) {
    # splitting into training and testing data and pulling cl out
    data_train <- data %>% filter(split != i)
    train_clean <- data_train %>% select(-split, -cl)
    cl <- data_train %>% pull(cl)
    data_test <- data %>% filter(split == i)
    test_clean <- data_test %>% select(-split, -cl)
    # finding k nearest neighbors
    p_class <- knn(train = train_clean,
                   test = test_clean,
                   cl = cl,
                   k = k_nn)
    # storing predictions
    predictions[[i]] <- data.frame(p_class)
    # calculating proportion of observations that were classified incorrectly
    miss_rate <- mean(ifelse(data_test$cl == p_class, 0, 1))
    # adding that number to table
    m_rate_df <- rbind(m_rate_df, i = miss_rate)
  }
  # getting rid of label for reading purposes
  colnames(m_rate_df) <- NULL
  # storing predicted class using all data
  class <- as.character(knn(train = train,
                            test = train,
                            cl = data %>% pull(cl),
                            k_nn))
  # finding average of all misscalculation errors
  cv_error <- colMeans(m_rate_df)
  # creaing list to output
  my_list <- list("class" = class,
                  "cv_err" = cv_error)
  return(my_list)
}
