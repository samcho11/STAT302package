#' T-test
#'
#' This function performs t-test for given value.
#'
#' @param x A numeric vector of data
#' @param alternative A character string specifying the alternative hypothesis.
#'                    This should only accept "two.sided", "less", or "greater".
#'                    Otherwise, your function should throw an informative error.
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @keywords inference
#'
#' @return A list consisting following elements:
#'         \code{test_stat} : The numeric test statistics
#'         \code{df} : The degree of freedom
#'         \code{alternative} : The value of parameter \code{alternative}
#'         \code{p_val} : The numeric p value
#' @examples
#' x <- rnorm(10, mean = 0, sd = 1)
#' my_t.test(x, alternative = "two.sided", mu = 0)
#'
#' @export
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
