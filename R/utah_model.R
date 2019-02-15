#' @title Utah Model
#' @name utah_model
#' @description Quantifies the chill accumulation by means of converting temperatures to chill-units.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on chill-units, where 1 chill-unit is when the tree is exposure between 2.4°C and 9.1°C, being the optimum temperature 6°C . When the temperature is between 9.1°C and 12.4°C, the chill-unit is 0.5. The chill-units accumulation is 0 when occurs temperature below 1.4 and between 12.5°C and 15.9°C. When the temperature is between 16°C and 18°C, the chill-unit is -0.5. When the temperature is above 18°C, the chill-unit is -1. 
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @examples 
#' 
#' x <- rnorm(500, 5, 3)
#' utah_model(x)
#' utah_model(x, FALSE)
#' 
#' @references Richardson, E, A. et al. 1974. "A Model for Estimating the Completation of Rest for 'Redhaven' and 'Elberta' Peach Trees". Research Reports & Notes. 
#' @importFrom utils tail
#' @export

utah_model <- function(x, total=TRUE){
  t <- c(1.4,2.4,9.1,12.4,15.9,18)
  v <- c(0,0.5,1,0.5,0,-0.5, -1)
  
  y <- rep(0, length(x))
  y[which(x<=t[1])] <- v[1]
  y[which(x>t[1] & x<=t[2])] <- v[2]
  y[which(x>t[2] & x<=t[3])] <- v[3]
  y[which(x>t[3] & x<=t[4])] <- v[4]
  y[which(x>t[4] & x<=t[5])] <- v[5]
  y[which(x>t[5] & x<=t[6])] <- v[6]
  y[which(x>t[6])] <- v[7]
  
  if (total == TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}