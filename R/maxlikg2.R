#' My first function
#'
#' @param theta1 A sample vector
#' @param theta2 A sample vector
#' @param lfun a function to be applied to both inputed theta samples
#'
#' @return a 2 parameters with a general log  likelihood  estimator
#' @export
#'
#' @examples
#' maxlikg2(theta1=seq(0,1,length=1000),theta2=seq(0,10,length=1000),nlevels=20)



maxlikg2 <- function(theta1,theta2,lfun="logbinpois",...){
  logbinpois=function(theta1,theta2) log(dbinom(5,size=10,prob=theta1)) + log(dbinom(7,size=8,prob=theta1))+ log(dpois(3,lambda=theta2))

  n1=length(theta1)
  n2=length(theta2)
  z=outer(theta1,theta2,lfun)
  contour(theta1,theta2,exp(z),...) # exp(z) gives the lik
  maxl=max(exp(z))    # max lik
  coord=which(exp(z)==maxl,arr.ind=TRUE)  # find the co-ords of the max
  th1est=theta1[coord[1]] # mxlik estimate of theta1
  th2est=theta2[coord[2]]
  abline(v=th1est,h=th2est)
  axis(3,th1est,round(th1est,2))
  axis(4,th2est,round(th2est,2),las=1)
  list(th1est=th1est,th2est=th2est)
}
