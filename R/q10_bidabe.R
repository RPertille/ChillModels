#' @title Bidabe Model
#' @name q10_bidabe
#' @description Quantifies the chill accumulation by means of converting temperatures to temperature coeficients (Q10).
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on temperature coeficients (Q10). The Q10 coeficient is variable for each specie. The model contains dinamic variables. This model is been aplied in the warm climates. Was based on an exponential function that decreases according to the increase in temperature.
#' @return The function returns values for each temperature of vector (Total = FALSE), or returns the accumulation (Total = TRUE).
#' @examples 
#' 
#' x <- rnorm(500, 10, 3)
#' q10_bidabe(x)
#' q10_bidabe(x, FALSE)
#' 
#' @references
#' 
#' BIDABE, B. 1967. Action de la temperature sur l' evolution des bourgeons de pommier et comparaison de methodes de controle de l' epoque de floraison. Annu. Physiol. Veg.
#' 
#' @importFrom utils tail
#' @export

q10_bidabe <- function(x, total=TRUE){
  cq10 = 3
  
  y <- rep(0, length(x))
  y <- (1/24)*cq10^(-x/10)
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
}