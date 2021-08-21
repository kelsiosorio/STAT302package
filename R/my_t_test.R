#' t-test function
#'
#' This function performs one sample t-test.
#'
#' @param x Numeric vector of data.
#' @param alternative A character string that specifies alternative hypothesis
#'   must be "less", "greater", or "two.sided".
#' @param mu Numeric input indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return A list with the following elements:
#'   test_stat: the numeric test statistic
#'   df: the degrees of freedom
#'   alternative: the value of the parameter \code{alternative}
#'   p_val: the numeric p-value.
#'
#' @examples
#' # Creating a random vector
#' set.seed(302)
#' random_vec <- rnorm(10, mean = 1.5, sd = 1)
#'
#' # Performing one sample two sided t-test
#' my_t.test(x = random_vec, "two.sided", mu = 3)
#'
#' # Performing one sample one sided t-test
#' my_t.test(x = random_vec, "greater", mu = 3)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  #saving standard error
  st_error <- stats::sd(x) / sqrt(length(x))
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
    p_value <- stats::pt(test_stat - mu, df, lower.tail = TRUE)
    my_t_list$p_value <- p_value
    # function for when alt = greater
  } else if (alternative == "greater") {
    # evaluating p_value and adding to list
    p_value <- stats::pt(test_stat - mu, df, lower.tail = FALSE)
    my_t_list$p_value <- p_value
    # function when alt is two sided
  } else if (alternative == "two.sided") {
    # evaluating p_value and adding to list
    p_val <- 2 * stats::pt(ab_of_test_stat, df, lower.tail = FALSE)
    my_t_list$p_val <- p_val
  } else {
    # message for when alt input is not one of the acceptable three
    message("*alternative* input must be less, greater, or two.sided!")
    return(alternative)
  }
  return(my_t_list)
}
