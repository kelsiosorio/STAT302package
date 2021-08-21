# within test-my_t.test.R
library(STAT302package)

# Creating a random vector
set.seed(302)
random_vec <- rnorm(10, mean = 1.5, sd = 1)

ran_test <- my_t.test(x = random_vec, "two.sided", mu = 3)

test_that("Expecting List", {
  expect_type(ran_test, "list")
  expect_type(my_t.test(x = random_vec, "less", mu = 3), "list")
  expect_type(my_t.test(x = random_vec, "greater", mu = 3), "list")
})

test_that("Correct outputs in the list", {
  expect_type(ran_test$test_stat, "double")
  expect_type(ran_test$df, "double")
  expect_type(ran_test$alternative, "character")
  expect_type(ran_test$p_val, "double")
})

test_that("The alternative input must be less, greater, or two.sided!", {
  expect_message(my_t.test(x = random_vec, "one", mu = 3), "input must be less, greater, or two.sided!")
})

