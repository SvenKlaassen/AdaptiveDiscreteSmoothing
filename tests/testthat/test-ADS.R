test_cases <- expand.grid(
  kernel =  c("gaussian","epa","unif","tri"),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

learnerlist <- list(mlr3::mlr_learners$get("regr.lm"),
                    mlr3::mlr_learners$get("regr.cv_glmnet"))

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

      model <- ADS(df = data,
                   target = "y",
                   individ = "ind",
                   learner = learner,
                   kernel = kernel)
      expect_length(model$learner_list,N)

      model_fit <- predict(model,newdata = data)
      expect_length(model_fit$fit,n*N)
      expect_equal(as.vector(model$fitted.values),as.vector(model_fit$fit))

      weight_plot <- autoplot(model)
      expect_class(weight_plot,"gg")
    }
  )
}

