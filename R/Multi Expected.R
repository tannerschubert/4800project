expmulti <- function(PDF, N, a, b, c, d){
  fxmax <- optimize(PDF, c(a,b), y=0, lower = a, upper = b,
                    maximum = TRUE)
  fymax <- optimize(PDF, c(c,d), x=0, lower = c, upper = d,
                    maximum = TRUE)
  fxymax <- PDF(fxmax$maximum, fymax$maximum)
  xsamp <- runif(N, a, b)
  ysamp <- runif(N, c, d)
  zsamp <- runif(N, 0, fxymax)
  estPDF <- data.frame(x=ifelse(xsamp[zsamp<PDF(xsamp,ysamp)], xsamp, NA), y=ifelse(ysamp[zsamp<PDF(xsamp,ysamp)], ysamp, NA))
  PDF(mean(estPDF$x), mean(estPDF$y))
}