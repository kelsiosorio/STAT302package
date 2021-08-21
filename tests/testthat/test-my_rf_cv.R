# within test-my_knn_cv.R

library(STAT302package)

test_that("Expecting List", {
  expect_type(my_rf_cv(5), "double")
})

test_that("non-numeric input throws error", {
  expect_error(my_rf_cv(lol))
})
