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
#' @return
#' @export
#'
ADS <- function(df,
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
    task_list <- future.apply::future_lapply(1:N, function(i) {
      data <-  dplyr::mutate(dplyr::select(df,-dplyr::all_of(c(individ))),
                             weight_ADS =  sapply(ind_index, function(j) W_path[i,j,it]))
      task <- mlr3::TaskRegr$new(id = level_vec[i], backend = data, target = target)
      #change role of weight
      #task$set_col_role("weight_ADS","weight")
      task$col_roles$feature <- task$col_roles$feature[-length(task$col_roles$feature)]
      task$col_roles$weight <- "weight_ADS"
      task
    },future.seed = T)
    #list of learners (with training)
    learner_list <- future.apply::future_lapply(1:N, function(i) {
      temp_learner <- learner$clone()
      temp_learner$train(task_list[[i]])
    },future.seed = T)
    #disable parallel processing
    if (parallel == T){
      future::plan(future::sequential)
    }

    #adjust weight matrix
    W_path[,,it + 1] <- delta[it]*sapply(1:N, function(i) {
      sapply(1:N,function(j) {
        calc_dist(model_1 = learner_list[[i]],model_2 = learner_list[[j]],
                  gamma = gamma[it], task_list=task_list, kernel = kernel)
      })
    })

    #set diag weight to 1
    diag(W_path[,,it + 1]) <- 1
  }

  #fitted values
  fit_list <- lapply(1:N, function(i) {
    learner_list[[i]]$predict(task_list[[i]], row_ids = (1:n)[ind_index == i])
  })

  #vector of fitted values
  fit <- rep(NA,n)
  invisible(sapply(1:N, function(i) fit[ind_index == i] <<- fit_list[[i]]$response))
  names(fit) <- 1:n
  input <- list("individ" = individ, "train_ids" = 1:n)
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
#' @return
#' @export
#'
predict.ADS <- function(object,
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


#' Plot a heatmap of the weight matrix.
#'
#'
#' @param object `ADS` object.
#' @param iterations Iterations of the weightmatrix to plot (1 equals the input matrix, default is diagonal.)
#'
#' @return
#'
#' @export
#' @importFrom ggplot2 autoplot
#'
autoplot.ADS <- function(object, iterations = 1:2){
  df_data <- reshape2::melt(object$Weight_path[,,iterations])
  heatmap <- ggplot2::ggplot(df_data, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill =.data$value)) +
    ggplot2::geom_tile() +
    ggplot2::xlab(label = "Covariates") +
    ggplot2::ylab(label = "Covariates") +
    ggplot2::scale_x_continuous(breaks = 1:length(object$level_ind)) +
    ggplot2::scale_y_continuous(breaks = 1:length(object$level_ind)) +
    ggplot2::facet_wrap(~ .data$Var3) +
    ggplot2::scale_fill_gradient(name = "Weight",low = "#FFFFFF",high = "#012345") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.placement = "outside",
          plot.title = ggplot2::element_text(hjust = 0.5),
          strip.background = ggplot2::element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
    ggplot2::ggtitle(label = "Weight Matrix") +
    ggplot2::theme(legend.position="bottom")
  return(heatmap)
}



