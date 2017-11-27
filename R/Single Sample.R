#' Single Variable Random Samples
#' 
#' Description
#' 
#' @param PDF a function to be sampled from
#' @param N number of attempted samples
#' @param a lower bound of the support of PDF
#' @param b upper bound of the support of PDF
#' 
#' @return a dataframe of samples from PDF
#' @export
#' 
#' @examples 
#' betaPDF <- function(x) {ifelse(0<x & x<1, 2*x, 0)}
#' svrs(betaPDF, 100, 0, 1)
#' 
#' unifPDF <- function(x) {ifelse(0<x & x<1, 1, 0)}
#' svrs(betaPDF, 100, 0, 1)
svrs <- function(PDF, N, a, b){
  fmax <- optimize(PDF, c(a,b), lower = a, upper = b,
                   maximum = TRUE)
  xsamp <- runif(N, a, b)
  ysamp <- runif(N, 0, fmax$objective)
  estpdf <- data.frame(x=ifelse(xsamp[ysamp<PDF(xsamp)], xsamp, NA))
}

