my_lm <- function(formula, data) {
  # creating matrices
  mat_x <- model.matrix(formula, data)
  mod_mat <- model.frame(formula, data)
  mat_y <- model.response(mod_mat)
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
  pr <- 2 * pt(ab_test_stat, df, lower.tail = FALSE)
  # adding Pr(>|t|) to table
  table1$pr <- pr
  # re-naming columns
  colnames(table1) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(table1)
}
