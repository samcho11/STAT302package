test_that("correct coefficients", {
  life_exp <- my_gapminder$lifeExp
  result <- my_lm(life_exp ~ gdpPercap + continent, data = my_gapminder)
  actual_result <- lm(life_exp ~ gdpPercap + continent, data = my_gapminder)
  expect_equal(result$Estimate.V1, actual_result$Estimate)
})
