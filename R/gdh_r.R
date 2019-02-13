#' @title GDH Model - Richardson - for heat accumulation
#' @name gdhr_model
#' @description Quantifies the Growing Degree Hours at between the base and optimum temperatures.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of GDH for each temperature (TRUE is default).
#' @details The GDH model is based on the subtraction of the base temperature of each hourly temperature between 4.5°C and 25°C. The temperature of the base is 4.5°C. Therefore, accumulation under temperatures below 4.5°C and above 25°C is zero.
#' @return The function returns values the GDH for each temperature of vector.
#' @examples 
#' 
#' x <- rnorm(500,8,3)
#' gdhr_model(x)
#' gdhr_model(x, FALSE)
#' 
#' @references 
#' 
#' RICHARDSON, E.A. et al. 1975. Pheno-climatographyof spring 249 peach bud development. HortScience.
#' 
#' @importFrom utils tail
#' @export

gdhr_model <- function(x, total=TRUE){
  tb <- 4.5
  
  y <- rep(0, length(x))
  y[which(x>tb)] <- x[which(x>tb)]-tb
  y[which(x>25)] <- 25-tb
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}