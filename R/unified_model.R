#' @title Unified Model
#' @name unified_model
#' @description Quantifies the chill accumulation rates by means of converting temperatures to chill-units. the coefficients used in the model are for the apple tree.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on chill-units, but the limits are unknown.
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @author Marcos Robson Sachet & Rafael Henrique Pertille.
#' @references Chuine, I. et al. 2016. Can phenological models predict tree phenology accurately in the future? The unrevealed hurdle of endodormancy break. Global Change Biology.
#' @importFrom utils tail
#' @export

unified_model <- function(x,total=TRUE){
  a1 = 0.89
  b1 = -28.87
  c1 = -19.44
  
  y <- rep(0, length(x))
  y <- 1/(1+exp((a1*(x-c1)*(x-c1)+b1*(x- c1))))
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}