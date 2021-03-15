my_t.test <- function(x,
                      alternative = "two.sided",
                      mu = 0) {
  # Verifies the alternative input
  if (alternative != "less" &&
      alternative != "greater" &&
      alternative != "two.sided") {
    stop("Invalid alternative hypothesis input")
  }

  # Calculate the test statistic, the T of the t test
  mean_x <- mean(x)
  n_x <- length(x)
  df = n_x - 1
  std_err_x <- sd(x) / sqrt(n_x)
  test_stat <- (mean_x - mu) / std_err_x

  # Calculate the p value according to the alternative hypothesis
  if (alternative == "two.sided") {
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE) * 2
  } else if (alternative == "greater"){
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE)
  } else {
    p_val <- pt(abs(test_stat), df, lower.tail = TRUE)
  }

  # Stop the function if the p-value is greater than 0
  if (p_val > 1) {
    stop("P-value cannot be greater than 0")
  }

  # Organize the calculated data into list
  my_list <- list("test_statistic" = test_stat,
                  "degree_freedom" = df,
                  "alternative_hypothesis" = alternative,
                  "p_value" = p_val)

  return(my_list)
}
