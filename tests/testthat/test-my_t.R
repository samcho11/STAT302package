# Test "two sided" with mu = 60
test_that("alternative equal to", {
  life_exp <- my_gapminder$lifeExp
  result <- my_t.test(life_exp, "two.sided", 60)
  actual_result <- t.test(life_exp, mu = 60, alternative = "two.sided")
  expect_equal(result$test_statistic, as.numeric(actual_result$statistic))
  expect_equal(result$degree_freedom, as.numeric(actual_result$parameter))
  expect_equal(result$p_value, 1 - as.numeric(actual_result$p.value))
})

# Test "less than" mu = 60
test_that("alternative less than", {
  life_exp <- my_gapminder$lifeExp
  result <- my_t.test(life_exp, "less", 60)
  actual_result <- t.test(life_exp, mu = 60, alternative = "less")
  expect_equal(result$test_statistic, as.numeric(actual_result$statistic))
  expect_equal(result$degree_freedom, as.numeric(actual_result$parameter))
  expect_equal(result$p_value, 1 - as.numeric(actual_result$p.value))
})

# Test "greater than" mu = 60
test_that("alternative greater than", {
  life_exp <- my_gapminder$lifeExp
  result <- my_t.test(life_exp, "greater", 60)
  actual_result <- t.test(life_exp, mu = 60, alternative = "greater")
  expect_equal(result$test_statistic, as.numeric(actual_result$statistic))
  expect_equal(result$degree_freedom, as.numeric(actual_result$parameter))
  expect_equal(result$p_value, 1 - as.numeric(actual_result$p.value))
})

test_that("error occured", {
  life_exp <- my_gapminder$lifeExp
  result <- my_t.test(life_exp, "two.sided", 60)
  error <- expect_equal(result$p_value, 1 - as.numeric(actual_result$p.value))
  expect_error(error)
})
