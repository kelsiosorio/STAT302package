my_t.test <- function(x, alternative, mu) {
  #saving standard error
  st_error <- sd(x) / sqrt(length(x))
  # creating test_stat and df object
  test_stat <- ((sum(x)/length(x)) - mu) / st_error
  df <- length(x) - 1
  # taking abs of test_Stat minus mu for future use
  ab_of_test_stat <- abs(test_stat)
  # Creating first part of list
  my_t_list <- list("test_stat" = test_stat,
                    "df" = df,
                    "alternative" = alternative)
  # function for when alt = less
  if (alternative == "less") {
    # evaluating p_value and adding to list
    p_value <- pt(test_stat - mu, df, lower.tail = TRUE)
    my_t_list$p_value <- p_value
    # function for when alt = greater
  } else if (alternative == "greater") {
    # evaluating p_value and adding to list
    p_value <- pt(test_stat - mu, df, lower.tail = FALSE)
    my_t_list$p_value <- p_value
    # function when alt is two sided
  } else if (alternative == "two.sided") {
    # evaluating p_value and adding to list
    p_val <- 2 * pt(ab_of_test_stat, df, lower.tail = FALSE)
    my_t_list$p_val <- p_val
  } else {
    # message for when alt input is not one of the acceptable three
    return(paste("The alternative input must be less, greater, or two.sided!"))
  }
  return(my_t_list)
}
