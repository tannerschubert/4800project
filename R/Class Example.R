#' SVRS
#' 
#' Description
#' 
#' @param f pdf sampled from
#' @param N number of attempted samples
#' @param low lower bound of the support of f
#' @param high upper
#' @param maxf bound of f
#' 
#' @return vector of samples from pdf
#' @export
#' 
#' @examples 
#' betaPDF <- function(x) {ifelse(0<x & x<1, 2*x, 0)}
#' class(betaPDF, 100, 0, 1, 2)

class <- function(f, N, low, high, maxf) {
  ones <- runif(N, low, high)
  unis <- runif(N, 0, maxf)
  ones[unis<f(ones)]
  return(plot(x=ones, y=unis))
}