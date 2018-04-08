#' @title Count between
#' @name count_between
#' @description Function to quantify temperature hours between x and y.
#' @param tli lower limit
#' @param tls upper limit
#' @param x Vector containing values (data).
#' @param total TRUE Shows the total value of hours.
#' @details Function to quantify temperature hours between x and y.
#' @return The function returns the total value of hours.
#' @author Marcos Robson Sachet & Rafael Henrique Pertille.
#' @importFrom utils tail
#' @export

count_between <- function(tli,tls, x, total=TRUE){
  y <- rep(0, length(x))
  y[which(x>=tli & x<tls)] <- 1
  if (total==TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}