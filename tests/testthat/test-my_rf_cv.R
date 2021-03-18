test_that("k = 2", {
  penguins <- na.omit(my_penguins)
  result_list <- list()
  for (i in 1:30) {
    result <- my_rf_cv(2)
    result_list(i) <- result
  }
})

test_that("k = 5", {
  penguins <- na.omit(my_penguins)
  result_list <- list()
  for (i in 1:30) {
    result <- my_rf_cv(2)
    result_list(i) <- result
  }
})

test_that("k = 10", {
  penguins <- na.omit(my_penguins)
  result_list <- list()
  for (i in 1:30) {
    result <- my_rf_cv(2)
    result_list(i) <- result
  }
})
