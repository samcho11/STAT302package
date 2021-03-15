my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Split data in k_cv parts, randomly
  k <- k_cv
  fold <- sample(rep(1:k, length = nrow(train)))
  split_data <- data.frame("x" = train, "split" = fold)
  split_class <- data.frame("y" = cl, "split" = fold)

  # Omit NA values
  #split_data <- na.omit(split_data)

  # Empty dataset for recording
  cv_err <- rep(NA, k_cv)

  # Iterate through k_cv times
  for (i in 1:k) {
    # Assign training and test variable
    data_train <- split_data %>% filter(split != i)
    data_train <- data_train %>% select(-split)

    data_test <- split_data %>% filter(split == i)
    data_test <- data_test %>% select(-split)

    class_train <- split_class %>% filter(split != i)
    class_train <- class_train %>% select(-split)

    class_test <- split_class %>% filter(split == i)
    class_test <- class_test %>% select(-split)

    result <- knn(data_train, data_test, class_train[, 1], k_nn)
    cv_err[i] <- mean(result != class_test[, 1])
  }
  full_knn <- knn(train, train, cl, k_nn)
  output_cv <- mean(cv_err)
  my_output <- list("class" = full_knn, "cv_err" = output_cv)
  return(my_output)
}
