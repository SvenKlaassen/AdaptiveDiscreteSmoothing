# test different inputs ####
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
                                       delta <- 0.7
                                       gamma <- 1
                                       iterations <- 1

                                       set.seed(42)
                                       model <- ADS$new(data = data,
                                                        target = "y",
                                                        individ = "ind",
                                                        delta = delta,
                                                        gamma = gamma,
                                                        iterations = iterations,
                                                        learner = learner,
                                                        W_start = diag(N),
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
                                       expect_length(model$task_list,iterations)
                                       for (it in 1:iterations){expect_length(model$task_list[[it]],N)}
                                       expect_length(model$learner_list,iterations)
                                       for (it in 1:iterations){expect_length(model$learner_list[[it]],N)}
                                       expect_array(model$weight_path, d = 3)
                                       expect_equal(dim(model$weight_path),c(N,N,iterations + 1))

                                       fit_1 <- model$predict(newdata = data)
                                       expect_equal(model$predictions, fit_1)

                                       #calculate mse
                                       mse <- model$calc_mse(newdata = data)$mse
                                       mse_pred <- colMeans((data$y-cbind(0,model$predictions))^2)
                                       names(mse_pred) <- seq(0,iterations, by = 1)
                                       expect_equal(mse_pred, mse, tolerance = 1e-2)

                                       set.seed(42)
                                       model_2 <- ADS_function(df = data,
                                                               target = "y",
                                                               individ = "ind",
                                                               delta = delta,
                                                               gamma = gamma,
                                                               iterations = iterations,
                                                               learner = learner,
                                                               W_start = diag(N),
                                                               kernel = kernel,
                                                               calc_dist = calc_dist_default)
                                       fit_2 <- predict_ADS(model_2, newdata = data)$fit

                                       mse_2 <- mean((data$y-fit_2)^2)
                                       names(mse_2) <- iterations
                                       expect_equal(mse_2, mse[iterations + 1], tolerance = 1e-1)

                                       #test plot_mse
                                       plt_1 <- model$plot_mse(newdata = data, interactive = FALSE)
                                       expect_identical(class(plt_1), c("gg", "ggplot"))
                                       plt_2 <- model$plot_mse(newdata = data, individuals = levels(ind)[1:2], interactive = FALSE)
                                       expect_identical(class(plt_2), c("gg", "ggplot"))

                                       #test heatmap
                                       heatmap_1 <- model$heatmap(interactive = FALSE)
                                       expect_identical(class(heatmap_1), c("gg", "ggplot"))
                                       heatmap_2 <- model$heatmap(iterations = 1, interactive = FALSE)
                                       expect_identical(class(heatmap_2), c("gg", "ggplot"))
                                     }
  )
}

# test different inputs ####
test_cases <- expand.grid(
  kernel =  c("gaussian","epa","unif","tri"),
  iterations = 2,
  delta = 0.7,
  gamma = 1,
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

      set.seed(42)
      model <- ADS$new(data = data,
                       target = "y",
                       individ = "ind",
                       delta = rep(delta,iterations),
                       gamma = rep(gamma,iterations),
                       iterations = iterations,
                       learner = learner,
                       W_start = diag(N),
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
      expect_length(model$task_list,iterations)
      for (it in 1:iterations){expect_length(model$task_list[[it]],N)}
      expect_length(model$learner_list,iterations)
      for (it in 1:iterations){expect_length(model$learner_list[[it]],N)}
      expect_array(model$weight_path, d = 3)
      expect_equal(dim(model$weight_path),c(N,N,iterations + 1))

      fit_1 <- model$predict(newdata = data)
      expect_equal(model$predictions, fit_1)

      #calculate mse
      mse <- model$calc_mse(newdata = data)$mse
      mse_pred <- colMeans((data$y-cbind(0,model$predictions))^2)
      names(mse_pred) <- seq(0,iterations, by = 1)
      expect_equal(mse_pred, mse, tolerance = 1e-2)

      set.seed(42)
      model_2 <- ADS_function(df = data,
                              target = "y",
                              individ = "ind",
                              delta = rep(delta,iterations),
                              gamma = rep(gamma,iterations),
                              iterations = iterations,
                              learner = learner,
                              W_start = diag(N),
                              kernel = kernel,
                              calc_dist = calc_dist_default)
      fit_2 <- predict_ADS(model_2, newdata = data)$fit

      mse_2 <- mean((data$y-fit_2)^2)
      names(mse_2) <- iterations
      expect_equal(mse_2, mse[iterations + 1], tolerance = 1e-1)

      #test plot_mse
      plt_1 <- model$plot_mse(newdata = data, interactive = FALSE)
      expect_identical(class(plt_1), c("gg", "ggplot"))
      plt_2 <- model$plot_mse(newdata = data, individuals = levels(ind)[1:2], interactive = FALSE)
      expect_identical(class(plt_2), c("gg", "ggplot"))

      #test heatmap
      heatmap_1 <- model$heatmap(interactive = FALSE)
      expect_identical(class(heatmap_1), c("gg", "ggplot"))
      heatmap_2 <- model$heatmap(iterations = 1, interactive = FALSE)
      expect_identical(class(heatmap_2), c("gg", "ggplot"))
    }
  )
}

# test error messages ####
#create data
n <- 20; N <- 5; p <- 2
X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
Y <- X%*%rep(1,p) + rnorm(n*N,0,1)
ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
data <- data.frame(X,"y" = Y, "ind" = ind)

model <- ADS$new(data = data,
                 target = "y",
                 individ = "ind",
                 delta = 0.7,
                 gamma = 1,
                 iterations = 2,
                 learner = mlr_learners$get("regr.lm"))
input = "input"
expect_error(model$data <- input)
expect_error(model$learner <- input)
expect_error(model$task_list <- input)
expect_error(model$learner_list <- input)
expect_error(model$delta <- input)
expect_error(model$gamma <- input)
expect_error(model$weight_path <- input)
