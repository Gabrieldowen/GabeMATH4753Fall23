#' My confidence interval function
#'
#' @param x a sample to get ci for
#'
#' @return a confidence interval
#' @export
#'
#' @examples
#' myci(x)
myci<-function(x) {
  t=qt(0.95,24)
  ci=c()
  ci[1]=mean(d)-t*sd(x)/sqrt(25)
  ci[2]=mean(d)+t*sd(x)/sqrt(25)
  print(ci)
}
