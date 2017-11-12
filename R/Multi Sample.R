#' Multivariable Random Samples
#' 
#' Description
#' 
#' @param PDF a function to be sampled from
#' @param N number of attempted samples
#' @param a lower bound of the x support of PDF
#' @param b upper bound of the y support of PDF
#' 
#' @return a dataframe of samples from PDF
#' @export
#' 
#' @examples 
#' ExPDF <- function(x) x+y
#' mvrs(ExPDF, 100, 0, 1, 0, 1)
mvrs <- function(PDF, N, a, b, c, d){
  fxmax <- optimize(PDF, c(a,b), y=0, lower = a, upper = b,
                   maximum = TRUE)
  fymax <- optimize(PDF, c(c,d), x=0, lower = c, upper = d,
                    maximum = TRUE)
  fxymax <- PDF(fxmax$maximum, fymax$maximum)
  xsamp <- runif(N, a, b)
  ysamp <- runif(N, c, d)
  zsamp <- runif(N, 0, fxymax)
  estPDF <- data.frame(x=ifelse(xsamp[zsamp<PDF(xsamp,ysamp)], xsamp, NA), y=ifelse(ysamp[zsamp<PDF(xsamp,ysamp)], ysamp, NA))
  return(estPDF)
}
