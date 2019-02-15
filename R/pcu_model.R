#' @title Positive Chill Units - PCU
#' @name pcu_model
#' @description Quantifies the chill accumulation by means of converting temperatures to positive chill-units - Modified Utah Model.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The PCU Model is the modified Utah Model. When the temperature is above 15.9°C, the chill-unit is 0. This modification was made because when the Utah model is applied in warm conditions, accumulation becomes negative.
#' @return The function returns values the chill-units for each temperature of vector.
#' @examples 
#' 
#' x <- rnorm(500, 15, 4)
#' pcu_model(x)
#' pcu_model(x, FALSE)
#' 
#' @references 
#' 
#' Richardson, E. A. et al. 1974. "A Model for Estimating the Completation of Rest for 'Redhaven' and 'Elberta' Peach Trees". Research Reports & Notes. 
#' 
#' Linsley-Noakes, G. C. et al. 1995. "Estimating daily positive Utah Chill units using daily minimum and maximum temperatures".
#' 
#' @importFrom utils tail
#' @export
pcu_model <- function(x, total=TRUE){
  t <- c(1.4,2.4,9.1,12.4)
  v <- c(0,0.5,1,0.5,0)
  
  y <- rep(0, length(x))
  y[which(x<=t[1])] <- v[1]
  y[which(x>t[1] & x<=t[2])] <- v[2]
  y[which(x>t[2] & x<=t[3])] <- v[3]
  y[which(x>t[3] & x<=t[4])] <- v[4]
  y[which(x>t[4])] <- v[5]
  
  if (total == TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}