svrs <- function(PDF, N, a, b){
  fmax <- optimize(PDF, c(a,b), lower = a, upper = b,
                   maximum = TRUE)
  xsamp <- runif(N, a, b)
  ysamp <- runif(N, 0, fmax$objective)
  estpdf <- ifelse(xsamp[ysamp<PDF(xsamp)], xsamp, NA)
}

