#' Linear model function
#'
#' This function fits a linear model.
#'
#' @param formula A \code{formula} class object.
#' @param data Input a data frame.
#' @keywords inference prediction
#'
#' @return A table with rows for each coefficient (including Intercept), and columns
#'   for the Estimate, Standard Error, t value, and Pr(>|t|).
#'
#' @examples
#' # Generating data for example
#' my_data <- data.frame("x" = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14),
#' "y" = c(5.34, 6.78, 4.78, 8.45, 5.81, 6.24, 6.93, 5.88, 6.46, 7.12))
#'
#' # Fitting model
#' my_lm(x ~ y, data = my_data)
#'
#' @export
my_lm <- function(formula, data) {
  # creating matrices
  mat_x <- stats::model.matrix(formula, data)
  mod_mat <- stats::model.frame(formula, data)
  mat_y <- stats::model.response(mod_mat)
  # calculating estimates
  b_hat <- solve(t(mat_x) %*% mat_x) %*% t(mat_x) %*% mat_y
  # creating first column in table
  table1 <- as.data.frame(b_hat)
  # calculating degrees of freedom
  df <- nrow(mat_x) - ncol(mat_x)
  # calculating sigma squared
  sigma_sqr <- sum((mat_y - (mat_x %*% b_hat))^2 / df)
  # calculating standard error
  std_error <- sqrt(diag(sigma_sqr * solve(t(mat_x) %*% mat_x)))
  # adding standard error to table
  table1$std_error <- std_error
  # calculate test statistic, t value
  test_stat <- b_hat/std_error
  # adding t value to table
  table1$t_value <- test_stat
  # calculating Pr(>|t|), two sided t-test
  ab_test_stat <- abs(test_stat)
  pr <- 2 * stats::pt(ab_test_stat, df, lower.tail = FALSE)
  # adding Pr(>|t|) to table
  table1$pr <- pr
  # re-naming columns
  colnames(table1) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(table1)
}


