#' @title GDH Model - Anderson - for heat accumulation
#' @name gdha_model
#' @description Quantifies the Growing Degree Hours at between the base and optimum temperatures, by Anderson et al. (1986).
#' @param x Vector containing temperature values (Celsius-degree).
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of GDH for each temperature (TRUE is default).
#' @details The GDH Model is based on base and optimum temperature. The base temperature is 4°C. The optimum and critical temperature are 25°C and 36°C, respectively.
#' @return The function returns values the GDH for each temperature of vector.
#' @examples 
#' 
#' x <- rnorm(500,8,3)
#' gdha_model(x)
#' gdha_model(x, FALSE)
#' 
#' @references
#' 
#' Anderson, J. L. et al. 1986. Validation of chill unit and flower bud phenology models for "Montmorency" sour cherry. Acta Horticulturae - Modelling in Fruit Research.
#' 
#' @importFrom utils tail
#' @export

gdha_model <- function(x, total=TRUE){
  tb <- 4
  tu <- 25
  tc <- 36
  a <- tu-tb
  f1 = 1
  
  y <- rep(0, length(x))
  y[which(x >= tb & x <= tu)] <- f1*a/2*(1+cos(pi+pi*(x[which(x>=tb & x<=tu)]-tb)/(tu-tb)))
  y[which(x>tu & x<=tc)] <- f1*a*(1+cos(pi/2+pi/2*(x[which(x>tu & x<=tc)]-tu)/(tc-tu)))
  
  if (total==TRUE) 
    return(tail(cumsum(y),n=1))
  else return(y)
  
}