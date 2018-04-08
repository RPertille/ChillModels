#' @title Count Up
#' @name count_up
#' @description Function to quantify temperature hours above x.
#' @param tl lower limit
#' @param x Vector containing values (data).
#' @param total TRUE Shows the total value of hours.
#' @details Function to quantify temperature hours above x.
#' @return The function returns the total value of hours.
#' @author Marcos Robson Sachet & Rafael Henrique Pertille.
#' @importFrom utils tail
#' @export

count_up <- function(tl, x, total=TRUE){
  y <- rep(0, length(x))
  y[which(x>tl)] <- 1
  if (total==TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}