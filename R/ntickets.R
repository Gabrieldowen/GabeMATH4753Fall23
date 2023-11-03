#' My first function
#'
#' @param N number of tickets on a plane
#' @param gamma the probability that the plane will be truly overbooked
#' @param p probability for each ticket holder to show up to the flight
#'
#' @return a vector of squared components
#' @export
#'
#' @examples
#' ntickets(N=400, gamma = 0.02, p = 0.95)
#' ntickets()

ntickets<-function(N=400, gamma = 0.02, p = 0.95){
  getwd()

  # Use Discrete Distribution
  i <- N
  while (TRUE) {
    output <- qbinom(1-gamma, i, p)
    #print(paste(pbinom(N, output, p) , N, output, p,"<?" , (1-gamma)))
    if (pbinom(N, output, p) <= 1-gamma) {
      nd <- output - 1
      break
    }

    i <- i + 1
  }

  # plot objective function vs n (discrete)
  library(ggplot2)
  df <- data.frame(x = N:(N*1.1))
  df$p <- 1- pbinom(N, df$x, p)

  plot1<-ggplot(data = df, aes(x = x, y = p)) +
    geom_line(color = "blue") +
    labs(x = "nd", y = "Objective Function") +
    geom_point(color = "blue", size = 1) +
    geom_vline(xintercept = nd, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 1- pbinom(N, nd, p), color = "red", linetype = "dashed") +
    ggtitle(paste("Objective Vs nd to find optimal tickets sold \n(", nd, ") gamma = ",
                  gamma, "N = ", N, "discrete")) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )

  # use normal approximation


  root <- function(x){
    #print(paste(N ,x, p))
    return(pnorm(N, x * p, sqrt(x * p * (1 - p))) -(1 - gamma))
    # where pnorm == 1-gamm
  }


  nc<-uniroot(root, c(N, N*1.5))$root



  plot2<-ggplot(data = df, aes(x = x, y = 1 - root(x)-1)) +
    geom_line(color = "blue") +
    labs(x = "nc", y = "Objective Function") +
    ggtitle(paste("Objective Vs nc to find optimal tickets sold \n(", nc, ") gamma = ",
                  gamma, "N = ", N, "continuous")) +
    geom_vline(xintercept = nc, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 1-root(nc)-1, color = "red", linetype = "dashed") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )

  val<-list( nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  # final output
  print(plot1)
  print(plot2)
  print(val)
}
