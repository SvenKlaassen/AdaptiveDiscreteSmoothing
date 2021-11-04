#' Distance between models.
#'
#' @param model_1 ([`LearnerRegr`][mlr3::LearnerRegr])\cr
#' First Model.
#' @param model_2 ([`LearnerRegr`][mlr3::LearnerRegr])\cr
#' Second Model.
#' @param task_list ([`TaskRegr`][mlr3::TaskRegr])\cr
#' Task list.
#' @param ... Additional arguments
#'
#' @return (`numeric(1)`) \cr
#' A distance between the two models.
#' @export
#'
calc_dist_default <- function(model_1, model_2, task_list,...){
  if (class(model_1)[1] == "LearnerRegrLM"){
    dist <- sum((model_1$model$coefficients-model_2$model$coefficients)^2, na.rm = TRUE)
  } else if (class(model_1)[1] == "LearnerRegrCVGlmnet") {
    dist <- sum((coef(model_1$model, s = "lambda.min")-coef(model_2$model, s = "lambda.min"))^2, na.rm = TRUE)
  } else {
    dist <- mean((model_1$predict(task_list[[1]])$response - model_2$predict(task_list[[1]])$response)^2)
  }
  return(dist)
}
