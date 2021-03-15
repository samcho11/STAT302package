my_lm <- function(formula, data) {
  # Extract the model matrix and model response
  X <- model.matrix(formula, data)
  Y <- model.response(model.frame(formula, data))

  # Calculate the linear regression coefficients
  beta_est <- solve(t(X) %*% X) %*% t(X) %*% Y

  # degree of Freedom
  df = nrow(data) - ncol(X)

  # Estimate the sigma squared
  Y_mat <- as.matrix(Y)
  Xb <- X %*% beta_est
  total <- 0
  for (i in 1 : nrow(data)) {
    total <- total + ((Y_mat[i, 1] - Xb[i, 1])^2 / df)
  }
  sigma_sqrd <- total

  # Estimate the standard error
  se <- sqrt(sigma_sqrd * solve(t(X) %*% X))
  se <- diag(se)

  # t value
  t_val <- beta_est / se

  # Calculate the area under the t-distribution
  p_val <- pt(abs(t_val), df, lower.tail = FALSE ) * 2

  # Organize the data into table
  library(data.table)
  result <- data.table(
    "Estimate" = beta_est,
    "Std. Error" = se,
    "t value" = t_val,
    "Pr(>|t|)" = p_val
  )

  return(result)
}
