#' Distance between models.
#'
#' @param model_1 First Model.
#' @param model_2 Second Model.
#' @param gamma Weighting of the distance.
#' @param task_list Task list.
#' @param kernel Kernel
#' @param ... Additional arguments
#'
#' @return
#' @export
#'
calc_dist_default <- function(model_1,model_2,gamma = 1,task_list,kernel = "gaussian",...){
  checkmate::assertChoice(kernel,c("gaussian","epa","unif","tri"))
  if (class(model_1)[1] == "LearnerRegrLM"){
    dist <- sum((model_1$model$coefficients-model_2$model$coefficients)^2)
  } else if (class(model_1)[1] == "LearnerRegrCVGlmnet") {
    dist <- sum((coef(model_1$model, s = "lambda.min")-coef(model_2$model, s = "lambda.min"))^2)
  } else {
    dist <- mean((model_1$predict(task_list[[1]])$response-model_2$predict(task_list[[1]])$response)^2)
  }
  if (kernel == "gaussian"){
    res <- sqrt(pi/gamma)*evmix::kdgaussian(x = dist, lambda = 1/(sqrt(2*gamma)))
  } else if (kernel == "epa") {
    res <- 4/(3*gamma)*evmix::kdepanechnikov(x = dist, lambda = 1/gamma)
  } else if (kernel == "unif") {
    res <- 2/gamma * evmix::kduniform(x = dist, lambda = 1/gamma)
  } else if (kernel == "tri") {
    res <- 1/gamma*evmix::kdtriangular(x = dist, lambda = 1/gamma)
  }
  return(res)
}
