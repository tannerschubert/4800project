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
svrs <- function(PDF, N, a, b, sampdist){
  if (sampdist=="unif") {
    max <- optimize(PDF, c(a,b), lower=a, upper = b, maximum=TRUE)
    fmax <- ceiling(max$objective)
    xsamp <- runif(2*fmax*N, a, b)
    ysamp <- runif(2*fmax*N, 0, fmax)
    estpdf <- data.frame(x=ifelse(ysamp<pdf(xsamp), xsamp, NA))
    estpdf <- as.data.frame(estpdf[complete.cases(estpdf),])
    colnames(estpdf) <- c("x")
    estpdf <- as.data.frame(estpdf$x[c(1:N)])
    colnames(estpdf) <- c("x")
    return(estpdf)
  } else if(sampdist=="exp") {
    max <- optimize(PDF, c(a,b), lower=a, upper = b, maximum=TRUE)
    fmax <- ceiling(max$objective)
    xsamp <- rexp(2*fmax*N, 1)
    ysamp <- runif(2*fmax*N, 0, fmax*dexp(xsamp, 1 ))
    estpdf <- data.frame(x=ifelse(ysamp<pdf(xsamp), xsamp, NA))
    estpdf <- as.data.frame(estpdf[complete.cases(estpdf),])
    colnames(estpdf) <- c("x")
    estpdf <- as.data.frame(estpdf$x[c(1:N)])
    colnames(estpdf) <- c("x")
    return(estpdf)
  } else return("Error, try choosing a known distribution")
  
}

