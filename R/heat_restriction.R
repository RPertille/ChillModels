#' @title Heat Restriction model
#' @name heat_restriction
#' @description Quantifies the chill accumulation rates by Utah Model and North Carolina Model, After a certain number of hours of heat.
#' @param x Vector containing temperature values (Celsius-degree).
#' @param model Model to be used for calculation. Use "utah" for Utah Model and "nc" for North Carolina Model.
#' @param nh Number of hours of continuous heat.
#' @param total TRUE Shows the total value of accumulation, FALSE shows the value of chill-unit for each temperature (TRUE is default).
#' @details The model is based on chill-units (Utah Model or North Carolina Model). After a certain number of hours of heat to stop counting the negative units. Negativation of the cold by the heat is restricted to a few days, 24h for Raseira (1982), 96h for Ebert (1986), 30 hours for Fishmann (1987) and 36 hours for Anzanello (2012).
#' @return The function returns values the chill-units for each temperature of vector (Total = FALSE), or returns the chill-units accumulation (Total = TRUE).
#' @examples 
#' heat_restriction(rep(19:26, 250), model = "nc", nh = 24)
#' @references Raseira (1982), Ebert (1986), Fishmann (1987) and Anzanello (2012).
#' @importFrom utils tail
#' @export

heat_restriction <- function(x,model, nh,total=TRUE){
  #x=vetor de temperaturas
  #modelo (Utah ou Carolina do Norte)
  # nh = numero de horas continuas com calor
  
  if (model=="utah"){
    y <- utah_model(x, total = F)
  }
  if (model=="nc"){
    y <- north_carolina(x, total=F)
  }
  
  z <- y
  z[which(z>=0)] <- 0
  z[which(z<0)] <- 1
  
  for(i in nh:length(z)){
    if(sum(z[(1+i-nh):i])==nh) y[i] <- 0
  }
  
  if (total==TRUE)
    return(tail(cumsum(y),n=1))
  else return(y)
}
