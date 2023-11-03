#' Lab 8 function
#'
#' @param n A sample size
#' @param iter number of iterations description
#' @return a histogram
#' @export
#'
#' @examples
#' GabesMyclt(n=10, iter=10000)
GabesMyclt<-function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  hist(sm)
}
