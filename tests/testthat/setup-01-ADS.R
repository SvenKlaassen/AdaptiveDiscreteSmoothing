#' Adaptive Discrete Smoothing Algorithm
#'
#' @param df A `data.frame` containing the covariates,target variable and a factor for the individuum.
#' @param target Name of the target column.
#' @param individ Name of the individuum column.
#' @param learner The specified learner.
#' @param delta Weighting of the distance.
#' @param gamma Weighting of the distance.
#' @param iterations Number of iteration
#' @param W_start The specified starting weights.
#' @param calc_dist The distance function.
#' @param kernel The kernel used in the distance function.
#' @param parallel Parallel execution
#' @param ... Additional arguments.
#'
#' @return An S3 object of class `ADS` with components
#' * `learnerlist` A list of the estimated learners.
#' * `weight_path` An array containing the different weight matrices.
#' * `fitted.values` The corresponding predictions.
#' * `level_ind` A vector containing the indications of the levels (of the individuals.)
#' * `task_list` A list containing the `mlr3` tasks.
#' * `fit_list` A list of the fits corresponding to the indiviuals.
#' * `input` A list of important inputs (the name of the column of the individuals.)
#'
#' @seealso `predict.ADS`, `autoplot.ADS`
#'
#' @examples
#' #create data
#' n <- 200; N <- 10; p <- 3
#' X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
#' ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
#'
#' #create 2 different types
#' Y <- X%*%rep(1,p) * (as.numeric(ind) <= N/2) + rnorm(n*N,0,1)
#'
#' data <- data.frame(X,"y" = Y, "ind" = ind)
#'
#' model <- ADS(df = data,target = "y",individ = "ind")
#'
#'
#'
ADS_function <- function(df,
                target,
                individ,
                learner = mlr3::mlr_learners$get("regr.lm"),
                delta = 0.7,
                gamma = 1,
                iterations = 2,
                W_start = NULL,
                calc_dist = calc_dist_default,
                kernel = "gaussian",
                parallel = F,...) {

  # Checking Arguments ####
  checkmate::assertDataFrame(df,any.missing = FALSE)
  checkmate::assertCharacter(target, min.len = 1,max.len = 1)
  checkmate::assertCharacter(individ, min.len = 1,max.len = 1)
  checkmate::assertNumeric(delta,lower = 0,upper = 1)
  if (length(delta) == 1){
    delta <- rep(delta,iterations)
  } else {
    checkmate::assertTRUE(length(delta) == iterations)
  }
  checkmate::assertNumeric(gamma,lower = 0)
  if (length(gamma) == 1){
    gamma <- rep(gamma,iterations)
  } else {
    checkmate::assertTRUE(length(gamma) == iterations)
  }

  #number of individuals
  ind <- df[,individ]
  checkmate::assertFactor(ind)
  level_vec <- levels(ind)
  N <- nlevels(ind)

  #vector of all observations and the corresponding number of the individual
  ind_index <- as.integer(1:N)[ind]
  #number of observations
  n <- dim(df)[1]

  #construct a path for the weight matrix
  W_path <- array(NaN, c(N,N,iterations+1))
  if (all(is.null(W_start))) {
    W_path[,,1] <- diag(N) #weight matrix for first stage
  } else {
    checkmate::assertMatrix(W_start, nrows = N, ncols = N)
    W_path[,,1] <- W_start
  }
  #enable parallel processing
  if (parallel == T){
    future::plan(future::multiprocess)
  }

  #start iterations
  for (it in 1:iterations){
    #list of tasks
    task_list <- future.apply::future_lapply(seq_len(N), function(i) {
      data <-  dplyr::mutate(dplyr::select(df,-dplyr::all_of(c(individ))),
                             weight_ADS =  vapply(ind_index, function(j) W_path[i,j,it], FUN.VALUE = numeric(1)))
      task <- mlr3::TaskRegr$new(id = level_vec[i], backend = data, target = target)
      #change role of weight
      #task$set_col_role("weight_ADS","weight")
      task$col_roles$feature <- task$col_roles$feature[-length(task$col_roles$feature)]
      task$col_roles$weight <- "weight_ADS"
      task
    },future.seed = T)
    #list of learners (with training)
    learner_list <- future.apply::future_lapply(seq_len(N), function(i) {
      temp_learner <- learner$clone()
      temp_learner$train(task_list[[i]])
    },future.seed = T)
    #disable parallel processing
    if (parallel == T){
      future::plan(future::sequential)
    }

    #adjust weight matrix
    W_path[,,it + 1] <- delta[it]*vapply(seq_len(N), function(i) {
      vapply(seq_len(N),function(j) {
        calc_dist(model_1 = learner_list[[i]],model_2 = learner_list[[j]],
                  gamma = gamma[it], task_list=task_list, kernel = kernel)
      }, FUN.VALUE = numeric(1))
    }, FUN.VALUE = numeric(N))

    #set diag weight to 1
    diag(W_path[,,it + 1]) <- 1
  }

  #fitted values
  fit_list <- lapply(seq_len(N), function(i) {
    learner_list[[i]]$predict(task_list[[i]], row_ids = seq_len(n)[ind_index == i])
  })

  #vector of fitted values
  fit <- rep(NA,n)
  invisible(lapply(seq_len(N), function(i){fit[ind_index == i] <<- fit_list[[i]]$response}))
  names(fit) <- seq_len(n)
  input <- list("individ" = individ)
  results <- list("learner_list" = learner_list,
                  "Weight_path" = W_path,
                  "fitted.values" = fit,
                  "level_ind" = level_vec,
                  "task_list" = task_list,
                  "fit_list" = fit_list,
                  "input" = input)
  class(results) <- "ADS"
  return(results)
}

#' Predict Method for ADS object.
#'
#' @param object ADS object.
#' @param newdata New dataframe.
#' @param ... Additional arguments
#'
#' @return A vector containig the predicted values.
#'
predict_ADS <- function(object,
                        newdata,
                        ...){
  checkmate::assertClass(object, "ADS")

  newdata[,object$input$individ] <- as.factor(newdata[,object$input$individ])
  newdata <- dplyr::mutate(newdata,"weight_ADS" = 1)
  #split newdata into lists based on individual
  newdata_list <-  split(dplyr::select(as.data.frame(newdata),-dplyr::all_of(c(object$input$individ))),
                         f = newdata[,object$input$individ])

  #predict values for each model
  fit_list <- lapply(levels(newdata[,object$input$individ]), function(level) {
    i <- match(level,object$level_ind)
    j <- match(level,levels(newdata[,object$input$individ]))
    predict(object = object$learner_list[[i]], newdata = newdata_list[[j]])
  })

  #reverse the split to make the vector compatible with newdata
  fit <- unsplit(fit_list, f = newdata[,object$input$individ])
  names(fit) <- 1:dim(newdata)[1]

  results <- list("fit" = fit)
  return(results)
}




