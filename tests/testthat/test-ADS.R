test_cases <- expand.grid(
  kernel =  c("gaussian","epa","unif","tri"),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")


patrick::with_parameters_test_that("Unit test for ADS",
  .cases = test_cases,
  {
    #create data
    n <- 20; N <- 10; p <- 3
    X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
    Y <- X%*%rep(1,p) + rnorm(n*N,0,1)
    ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
    data <- data.frame(X,"y" = Y, "ind" = ind)

    model <- ADS(df = data,
                 target = "y",
                 individ = "ind",
                 kernel = kernel)
    expect_length(model$learner_list,N)

    model_fit <- predict(model,newdata = data)
    expect_length(model_fit$fit,n*N)
    expect_equal(model$fitted.values,model_fit$fit)
  }
)


