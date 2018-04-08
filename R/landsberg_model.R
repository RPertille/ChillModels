#' @title Landsberg Model
#' @name landsberg_model
#' @description Quantifies the chill accumulation rates based in the base temperature.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on the subtraction of the base temperature of each hourly temperature. The temperature of the base is 5°C. 
#' @return The function returns values the chill for each temperature of vector (Total = FALSE), or returns the chill accumulation (Total = TRUE).
#' @author Marcos Robson Sachet & Rafael Henrique Pertille.
#' @references I don't know.!!!
#' @importFrom utils tail
#' @export

landsberg_model <- function(x, total=TRUE){
  tb <- 5
  
  y <- rep(1, length(x))
  y[which(x>tb)] <- tb/x[which(x>tb)]
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}