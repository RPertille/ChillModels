#' @title Dynamic Model
#' @name dynamic_model
#' @description Quantifies the chill accumulation with dynamic equation (two-steps).
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill for each temperature (TRUE is default).
#' @details The model is based on dynamic accumulation, by means of the relationship between temperatures. The dynamic model assumes that the accumulated chill-units are not annulled by high temperatures.
#' @return The function returns values the chill for each temperature of vector (Total = FALSE), or returns the chill accumulation (Total = TRUE).
#' @examples
#' 
#' x <- rnorm(500, 10, 5)
#' dynamic_model(x)
#' dynamic_model(x, FALSE)
#' 
#' @note Code adapted from the function \code{\link[chillR]{Dynamic_Model}}, of the \href{https://CRAN.R-project.org/package=chillR}{chillR} Package
#' 
#' 
#' @references
#' 
#' FISHMAN, Svetlana, EREZ, A. & COUVILLON, G. A. (1987). The Temperature Dependence of Dormancy Breaking in Plants: Computer Simulation of Processes Studied Under Controlled Temperatures. J. Theor. Biol.
#' 
#' LUEDELING, Eike  (2018). chillR: Statistical Methods for Phenology Analysis in Temperate Fruit Trees. R package version 0.70.12. \url{https://CRAN.R-project.org/package=chillR}
#' 
#' @importFrom utils tail
#' @export

dynamic_model <- function(x,total=TRUE){
  
  e0 <- 4153.5
  e1 <- 12888.8
  a0 <- 139500
  a1 <- 2.567e+18
  slp <- 1.6
  tetmlt <- 277
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