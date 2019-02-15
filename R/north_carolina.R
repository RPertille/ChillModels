#' @title North Carolina
#' @name north_carolina
#' @description Quantifies the chill accumulation by means of converting temperatures to chill-units.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on chill-units and optimum temperature, minimum and maximum temperature limits (They aren't classes), where 1 chill-unit is when the tree is exposure at optimum temperature (7.2°C). When the temperature is above 23°C (maximum temperature limit), the chill-unit is -2. The chill-units accumulation is 0 when occurs temperature below -1.1°C, being the miminum temperature limit.
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @examples 
#' 
#' x <- rnorm(500, 5, 4)
#' north_carolina(x)
#' north_carolina(x, FALSE)
#' 
#' @references 
#' 
#' SHALTOUT, Assem D. & UNRATH, C. R. 1983. Rest Completion Prediction Model for 'Starkrimson Delicious' Apples. J. Amer. Soc. Hort. Sci.
#' 
#' @importFrom utils tail
#' @export

north_carolina <- function(x, total=TRUE){
  tls <- 23.3
  to <- 7.2
  tli <- -1.1
  
  y <- rep(0, length(x))
  y[which(x>tls)] <-  -2
  y[which(x>to & x<=tls)] <- -0.0113*x[which(x>to & x<=tls)]^2+0.1638*x[which(x>to & x<=tls)]+0.3702
  y[which(x>tli & x<=to)] <- -0.0116*x[which(x>tli & x<=to)]^2+ 0.191*x[which(x>tli & x<=to)]+ 0.224
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}