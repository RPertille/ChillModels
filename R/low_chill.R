#' @title Low Chill
#' @name lowchill_model
#' @description Quantifies the chill accumulation by means of converting temperatures to chill-units.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on chill-units, where 1 chill-unit is when the tree is exposure between 1.8°C and 8°C. When the temperature is above 19.5°C, the chill-unit is -1. The chill-units accumulation is 0 when occurs temperature below -1°C and between 14°C and 17°C.
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @examples 
#' 
#' x <- rnorm(500, 12, 5)
#' lowchill_model(x)
#' lowchill_model(x, FALSE)
#' 
#' @references 
#' 
#' GILREATH, Phyllis R. & BUCHANAN, D. W. (1981). Rest Prediction Model for Low-chilling 'Sungold' Nectarine. J. Amer. Soc. Hort. Sci.
#' 
#' @importFrom utils tail
#' @export

lowchill_model <- function(x, total=TRUE){
  
  t <- c(-1,1.8,8,14,17,19.5)
  v <- c(0,0.5,1,0.5,0,-0.5,-1)
  
  y <- rep(v[7], length(x))
  y[which(x<=t[1])] <- v[1]
  y[which(x>t[1] & x<=t[2])] <- v[2]
  y[which(x>t[2] & x<=t[3])] <- v[3]
  y[which(x>t[3] & x<=t[4])] <- v[4]
  y[which(x>t[4] & x<=t[5])] <- v[5]
  y[which(x>t[5] & x<=t[6])] <- v[6]
  
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}