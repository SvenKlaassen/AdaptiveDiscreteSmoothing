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
      n <- 20; N <- 5; p <- 2
      X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
      Y <- X%*%rep(1,p) + rnorm(n*N,0,1)
      ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
      data <- data.frame(X,"y" = Y, "ind" = ind)

      #parameters
      delta = 0.7
      gamma = 1
      iterations = 2
      model <- ADS$new(data = data,
                       target = "y",
                       individ = "ind",
                       learner = learner,
                       delta = 0.7,
                       gamma = 1,
                       iterations = iterations,
                       calc_weight = list("fun" = calc_weight_default,
                                          "params" = list("kernel" = kernel)))

      #check acitve fields
      expect_output(model$print())
      expect_equal(model$data,data)
      expect_equal(model$learner$id,learner$id)
      expect_equal(model$delta, rep(delta,iterations))
      expect_equal(model$gamma, rep(gamma,iterations))

      #check fiting
      model$fit(store_predictions = TRUE)
      expect_length(model$task_list,N)
      expect_length(model$learner_list,N)

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
  delta = 0.7,
  gamma = 1,
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

for (learner in learnerlist){
  patrick::with_parameters_test_that("Unit test for ADS",
    .cases = test_cases,
    {
      #create data
      n <- 20; N <- 5; p <- 2
      X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
      Y <- X%*%rep(1,p) + rnorm(n*N,0,1)
      ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
      data <- data.frame(X,"y" = Y, "ind" = ind)

      model <- ADS$new(data = data,
                       target = "y",
                       individ = "ind",
                       delta = rep(delta,iterations),
                       gamma = rep(gamma,iterations),
                       iterations = iterations,
                       learner = learner,
                       W_start = diag(N))
      #check acitve fields
      expect_output(model$print())
      expect_equal(model$data,data)
      expect_equal(model$learner$id,learner$id)
      expect_equal(model$delta, rep(delta,iterations))
      expect_equal(model$gamma, rep(gamma,iterations))

      #check fiting
      model$fit(store_predictions = TRUE)
      expect_length(model$task_list,N)
      expect_length(model$learner_list,N)

      fit_1 <- model$predict(newdata = data)

      model_2 <- ADS_function(df = data,
                              target = "y",
                              individ = "ind",
                              delta = rep(delta,iterations),
                              gamma = rep(gamma,iterations),
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
