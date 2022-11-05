#' @noRd
#' @importFrom stats pf

aov_aggstats <- function(m, sd, n, full_output=TRUE){
  # https://rdrr.io/cran/rpsychi/src/R/ind.oneway.second.R
  if(length(n)==1){
    n <- rep(n, length(m))
  }


  k  <- length(m)           #number of groups
  Xg  <- sum(n*m)/sum(n)

  dfb <- k - 1              #degree of freedom
  dfw   <- sum(n) - k       #degree of freedom

  MSb <- sum(n * (m - Xg)^2)/(k-1)  #MS between
  MSw <-  sum((n-1)*sd^2)/dfw       #MS within
  if (full_output){
    SSb <- dfb * MSb
    SSw <- dfw * MSw
    SSt <- SSb + SSw
  }
  f.value <- MSb/MSw                #f value
  p.value <-  stats::pf(f.value, df1=dfb, df2=dfw, lower.tail = FALSE, log.p=FALSE)
  out <- data.frame(F=f.value, pval=p.value)

  if (full_output){
    add <- data.frame(MSb=MSb, MSw=MSw, SSb=SSb, SSw=SSw, SSt=SSt, dfb=dfb, dfw=dfw, dft=dfb+dfw)
    out <- cbind(out, add)
  }
  return(out)

}
