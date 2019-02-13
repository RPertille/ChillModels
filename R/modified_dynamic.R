#' @title Dynamic Model Modified
#' @name modified_dynamic
#' @description Quantifies the chill accumulation rates with modified dynamic equation for subtropical climate conditions .
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill for each temperature (TRUE is default).
#' @details The model is based on dynamic accumulation, by means of the relationship between temperatures. It is applicable in subtropical climate conditions. This model was calibrated and validated based on observations of orchards in Palmas - PR, Brazil.
#' @return The function returns values the chill for each temperature of vector (Total = FALSE), or returns the chill accumulation (Total = TRUE).
#' @examples 
#' 
#' x <- rnorm(500, 15, 8)
#' modified_dynamic(x)
#' modified_dynamic(x, FALSE)
#' 
#' @author Marcos Robson Sachet, Idemir Citadin & Rafael Henrique Pertille
#' @references
#' 
#' FISHMAN, Svetlana, EREZ, A. & COUVILLON G. A. (1987). The Temperature Dependence of Dormancy Breaking in Plants: Computer Simulation of Processes Studied Under Controlled Temperatures. J. Theor. Biol.
#' 
#' @importFrom utils tail
#' @export

modified_dynamic <- function(x,total=TRUE){
  e0 <- 4916.91140203932
  e1 <- 14181.7713569974
  a0 <- 129964.90415947
  a1 <- 2230788975077541888
  slp <- 1.32092446684837
  tetmlt <- 278.283755346682
  aa <- a0/a1
  ee <- e1 - e0
  TK <- x + 273
  ftmprt <- slp * tetmlt * (TK - tetmlt)/TK
  sr <- exp(ftmprt)
  xi <- sr/(1 + sr)
  xs <- aa * exp(ee/TK)
  ak1 <- a1 * exp(-e1/TK)
  interE <- 0
  memo <- new.env(hash = TRUE)
  posi <- 1
  assign(x = paste(1), value = 0, envir = memo)
  E = 0
  S <- ak1
  S[1] <- 0
  E <- S
  options(scipen = 30)
  for (l in 2:length(x)) {
    if (E[l - 1] < 1) {
      S[l] <- E[l - 1]
      E[l] <- xs[l] - (xs[l] - S[l]) * exp(-ak1[l])
    }
    else {
      S[l] <- E[l - 1] - E[l - 1] * xi[l - 1]
      E[l] <- xs[l] - (xs[l] - S[l]) * exp(-ak1[l])
    }
  }
  interE <- E
  y <- rep(0, length(x))
  y[which(interE >= 1)] <- interE[which(interE >= 1)] *
    xi[which(interE >= 1)]
  if (total == TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}