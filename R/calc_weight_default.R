#' Calculate weights from the distance.
#'
#' @param dist (`numeric(1)`) \cr
#' A distance.
#' @param delta (`numeric(1)`) \cr
#' Scaling of the distance (bandwidth of the kernel).
#' @param gamma (`numeric(1)`) \cr
#' Final scaling of the weight.
#' @param kernel (`character(1)`) \cr
#' A kernel of "gaussian", "epa", "unif", "tri".
#' @param ... Additional arguments
#'
#' @return  (`numeric(1)`) \cr
#' A weight corresponding to the distance
#' @export
calc_weight_default <- function(dist, delta = 0.7, gamma = 0.5, kernel = "gaussian", ...){
  checkmate::assertChoice(kernel,c("gaussian","epa","unif","tri","quart","paper"))
  scaled_dist <- dist/gamma
  if (kernel == "gaussian"){
    weight <- exp(-scaled_dist^2/2)
  } else if (kernel == "epa") {
    weight <- vapply(scaled_dist, function(x){max(1-x^2,0)},FUN.VALUE = numeric(1))
  } else if (kernel == "unif") {
    weight <- 2*ifelse(scaled_dist <= 1, 1/2, 0)
  } else if (kernel == "tri") {
    weight <- vapply(scaled_dist, function(x){max(1-x,0)},FUN.VALUE = numeric(1))
  } else if (kernel == "quart") {
    weight <- vapply(scaled_dist, function(x){max((1-x^2)^2,0)},FUN.VALUE = numeric(1))
  } else if (kernel == "paper"){
    weight <- exp(-gamma*dist)
  }
  return(delta*weight)
}

