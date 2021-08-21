# within test-my_lm.R
library(STAT302package)


my_data <- data.frame("x" = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14),
                      "y" = c(5.34, 6.78, 4.78, 8.45, 5.81, 6.24, 6.93, 5.88, 6.46, 7.12))
#Fitting model
data_lm <- my_lm(x ~ y, data = my_data)

test_that("Expecting List", {
  expect_type(data_lm, "list")
})


test_that("Numeric outputs", {
  expect_type(data_lm[1, 1], "double")
})
