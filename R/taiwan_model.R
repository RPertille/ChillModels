#' @title Taiwan model
#' @name taiwan_model
#' @description Quantifies the chill accumulation by means of converting temperatures to chill-units.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on chill-units, where 1 chill-unit is when the tree is exposure below 7.2°C. When the temperature is between 15.1°C and 26.6°C, there isn't accumulation of chill-unit. The chill-units accumulation is negative when occurs temperature above 26.7°C, and the chill-unit is -1 when occurs temperature above 27.8°C.
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @examples 
#' 
#' x <- rnorm(500, 20, 7)
#' taiwan_model(x)
#' taiwan_model(x, FALSE)
#' 
#' @references 
#' 
#' LU, M. T. et al. 2012. A model for estimating chilling requirement of very low-chill peaches in Taiwan. Acta Horticulturae, n. 962, p. 245.
#' 
#' @importFrom utils tail
#' @export

taiwan_model <- function(x, total=TRUE){
  
  t <- c(7.2,15,26.6,27.8)    #faixas de temperatura
  v <- c(1,0.5,0,-0.5,-1)     #valor de cada faixa
  
  y <- rep(v[5], length(x))
  
  y[which(x<=t[1])] <- v[1]
  y[which(x>t[1] & x<=t[2])] <- v[2]
  y[which(x>t[2] & x<=t[3])] <- v[3]
  y[which(x>t[3] & x<=t[4])] <- v[4]
  y[which(x>t[4])] <- v[5]
  if (total==TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}
