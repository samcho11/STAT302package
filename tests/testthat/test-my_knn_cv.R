test_that("using 5 folds", {
  covariates <- my_penguins[, 3:6]
  cl <- my_penguins$species
  result <- my_knn_cv(covariates, cl, 10, 5)
})
