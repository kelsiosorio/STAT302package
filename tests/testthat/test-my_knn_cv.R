# within test-my_knn_cv.R
library(STAT302package)
# Getting data ready
data("my_penguins")
data_clean <- stats::na.omit(my_penguins %>% dplyr::select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g))

# filtering for training data
p_train <- data_clean %>% dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Using function
knn_1 <- my_knn_cv(p_train, data_clean$species, 1, 5)

test_that("Expecting List", {
  expect_type(knn_1, "list")
})
