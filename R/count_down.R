#' @title Count Down
#' @name count_down
#' @description Function to quantify temperature hours below x.
#' @param tl upper limit
#' @param x Vector containing values (data).
#' @param total TRUE Shows the total value of hours.
#' @details Function to quantify temperature hours below x.
#' @return The function returns the total value of hours.
#' @examples 
#' 
#' count_down(tl = 7.2, x = rnorm(1000, 15, 5), total = TRUE)
#' 
#' @author Marcos Robson Sachet & Rafael Henrique Pertille.
#' @importFrom utils tail
#' @export

count_down <- function(tl, x, total=TRUE){
  y <- rep(0, length(x))
  y[which(x<=tl)] <- 1
  if (total==TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}