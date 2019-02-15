#' @title Jones Model
#' @name jones_model
#' @description Quantifies the chill accumulation by means of converting temperatures to exponential units temperature.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill for each temperature (TRUE is default).
#' @return The function returns values the chill for each temperature of vector (Total = FALSE), or returns the chill accumulation (Total = TRUE).
#' @examples 
#'  
#'  x <- rnorm(500, 8, 5)
#' jones_model(x)
#' jones_model(x, FALSE) 
#'  
#' @references 
#' 
#' JONES, H. G., HILLIS, R. M., GORDON, S. L., and BRENNAN, R. M. (2013). An approach to the determination of winter chill requirements for different Ribes cultivars. Plant. Biol.
#' 
#' @importFrom utils tail
#' @export

jones_model <- function(x, total=TRUE){
  
  y <- rep(0, length(x))
  y <- 0.6702*exp(-0.148*x)
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}