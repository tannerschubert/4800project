mvrs <- function(PDF, N, a, b, c, d){
  fxmax <- optimize(PDF, c(a,b), lower = a, upper = b,
                   maximum = TRUE)
  fymax <- optimize(PDF, c(c,d), lower = c, upper = d,
                    maximum = TRUE)
  fxymax <- PDF(fxmax, fymax)
  xsamp <- runif(N, a, b)
  ysamp <- runif(N, c, d)
  zsamp <- runif(N, 0, fxymax)
  estPDF <- if(zsamp<PDF(xsamp, ysamp)) {c(xsamp, ysamp)} else(c(NA,NA))
  Samples <- data.frame(t(estPDF))
}

exPDF <- function(x,y) {ifelse(0<x & x<1, x, 0) & ifelse(0<y & y<1, y, 0); x+y}
