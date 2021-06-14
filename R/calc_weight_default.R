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
  checkmate::assertChoice(kernel,c("gaussian","epa","unif","tri"))
  if (kernel == "gaussian"){
    weight <- sqrt(pi/gamma)*evmix::kdgaussian(x = dist, lambda = 1/(sqrt(2*gamma)))
  } else if (kernel == "epa") {
    weight <- 4/(3*gamma)*evmix::kdepanechnikov(x = dist, lambda = 1/gamma)
  } else if (kernel == "unif") {
    weight <- 2/gamma * evmix::kduniform(x = dist, lambda = 1/gamma)
  } else if (kernel == "tri") {
    weight <- 1/gamma*evmix::kdtriangular(x = dist, lambda = 1/gamma)
  }
  return(delta*weight)
}

