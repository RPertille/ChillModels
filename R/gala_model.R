#' @title Gala model
#' @name gala_model
#' @description Quantifies the chill accumulation rates by means of converting temperatures to chill-units.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on chill-units, where 1 chill-unit is when the tree is exposure between -2.1°C and 5.5°C. When the temperature is above 18°C, the chill-unit is -1. The chill-units accumulation is 0 when occurs temperature between 13°C and 16°C.
#' @note This model was make for quantifying the chill accumulation rates to 'Gala' apple or varieties of the Gala group. We aren't recommended the apliccation for others species or others groups of the apple tree.
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @examples
#' 
#' gala_model(rep(3:15,100))
#' 
#' @references GUAK, Sunghee & NEILSEN, Denise. (2013). Chill Unit Models for Predicting Dormancy Completion of Floral Buds in Apple and Sweet Cherry.
#' @importFrom utils tail
#' @export

gala_model <- function(x, total=TRUE){
  
  t <- c(-2.1,5.5,7,9,13,16,18)
  v <- c(0,1,0.75,0.5,0.25,0,-0.5,-1)
  
  y <- rep(v[8], length(x))
  y[which(x<=t[1])] <- v[1]
  y[which(x>t[1] & x<=t[2])] <- v[2]
  y[which(x>t[2] & x<=t[3])] <- v[3]
  y[which(x>t[3] & x<=t[4])] <- v[4]
  y[which(x>t[4] & x<=t[5])] <- v[5]
  y[which(x>t[5] & x<=t[6])] <- v[6]
  y[which(x>t[6] & x<=t[7])] <- v[7]
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}