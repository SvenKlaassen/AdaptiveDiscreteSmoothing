test_cases <- expand.grid(
  kernel =  c("gaussian","epa","unif","tri"),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

learnerlist <- list(mlr_learners$get("regr.lm"),
                    mlr_learners$get("regr.cv_glmnet"),
                    mlr_learners$get("regr.rpart"))

for (learner in learnerlist){
  patrick::with_parameters_test_that("Unit test for ADS",
    .cases = test_cases,
    {
      #create data
      n <- 200; N <- 10; p <- 3
      X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
      Y <- X%*%rep(1,p) + rnorm(n*N,0,1)
      ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
      data <- data.frame(X,"y" = Y, "ind" = ind)

      model <- ADS$new(data = data,
                       target = "y",
                       individ = "ind",
                       learner = learner,
                       calc_weight = list("fun" = calc_weight_default,
                                          "params" = list("kernel" = kernel)))
      model$fit(store_predictions = TRUE)
      expect_output(model$print())
      fit_1 <- model$predict(newdata = data)

      model_2 <- ADS_function(df = data,
                              target = "y",
                              individ = "ind",
                              learner = learner,
                              kernel = kernel)
      fit_2 <- predict_ADS(model_2, newdata = data)$fit

      #test heatmap
      plot <- model$heatmap()
      expect_identical(class(plot), c("gg", "ggplot"))
      plot2 <- model$heatmap(iterations = 1)
      expect_identical(class(plot2), c("gg", "ggplot"))
    }
  )
}



test_cases <- expand.grid(
  kernel =  c("gaussian","epa","unif","tri"),
  iterations = c(2,4),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

for (learner in learnerlist){
  patrick::with_parameters_test_that("Unit test for ADS",
    .cases = test_cases,
    {
      #create data
      n <- 200; N <- 10; p <- 3
      X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
      Y <- X%*%rep(1,p) + rnorm(n*N,0,1)
      ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
      data <- data.frame(X,"y" = Y, "ind" = ind)

      model <- ADS$new(data = data,
                       target = "y",
                       individ = "ind",
                       delta = rep(0.7,iterations),
                       gamma = rep(0.3,iterations),
                       iterations = iterations,
                       learner = learner,
                       W_start = diag(N))
      model$fit(store_predictions = TRUE)
      #expect_output(model$print())
      fit_1 <- model$predict(newdata = data)

      model_2 <- ADS_function(df = data,
                              target = "y",
                              individ = "ind",
                              delta = rep(0.7,iterations),
                              gamma = rep(0.3,iterations),
                              iterations = iterations,
                              learner = learner,
                              W_start = diag(N))
      fit_2 <- predict_ADS(model_2, newdata = data)$fit

      #test heatmap
      plot <- model$heatmap()
      expect_identical(class(plot), c("gg", "ggplot"))
      plot2 <- model$heatmap(iterations = 1)
      expect_identical(class(plot2), c("gg", "ggplot"))
    }
  )
}
