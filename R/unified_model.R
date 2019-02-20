#' @title Unified Model
#' @name unified_model
#' @description Quantifies the chill accumulation by means of converting temperatures to chill-units.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @param a Parameter defined by the user as the function limits. Default is 0.89.
#' @param b Parameter defined by the user as the function limits. Default is -28.87.
#' @param c Parameter defined by the user as the function limits. Default is -19.44.
#' @details The coefficients used in this model are adjusted for the apple tree.The model is based on chill-units, but the limits are unknown.
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @examples 
#' 
#' x <- rnorm(500,10,3)
#' unified_model(x)
#' unified_model(x, FALSE)
#' 
#' @references 
#' 
#' Chuine, I. et al. 2016. Can phenological models predict tree phenology accurately in the future? The unrevealed hurdle of endodormancy break. Global Change Biology.
#' 
#' Chuine, Isabelle. 2000. A unified model for budburst of trees. Journal of Theoretical Biology
#' 
#' @importFrom utils tail
#' @export

unified_model <- function(x, a = 0.89, b = -28.87, c = -19.44,  total=TRUE){
  if(a > 10 || a < 0) {
    warning("The limits in the 'a' argument was ultrapassed")
  }
  if(b > 15 || b < -30) {
    warning("The limits in the 'b' argument was ultrapassed")
  }
  

  y <- rep(0, length(x))
  y <- 1/(1+exp((a*(x-c)*(x-c)+b*(x- c))))
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}
