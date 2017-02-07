#' Convert a point-biserial correlation to a biserial correlation
#'
#' This function allows you to to convert a point-biserial correlation to a biserial correlation
#' @param Rpb A point-biserial correlation
#' @param p The proportion of "1's" in the dichotomous variable
#' @keywords Biserial correlation, point-biserial
#' RpbtoRbis()

RpbtoRbis <- function(Rpb,p)
{
  Rbis <- Rpb*((sqrt(p*(1-p)))/(dnorm(qnorm(p))))
  return(Rbis)
}




