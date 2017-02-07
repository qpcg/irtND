#' Increment Function
#'
#' This function allows you to increment a variable by 1, or a specified value. If the variable name does not exist, the function creates it and sets the value to one.
#' @param x The variable name you want to increment.
#' @param v Value to increment x by. Does not necesssarily need to be an integer. Default = 1.
#' @keywords Increment
#' i()

i <- function(x,v=1,envir = parent.frame()){
  name <- deparse(substitute(x))
  if(!exists(name, envir)){
    assign(name,1,envir) 
  } else{
  assign(name,x + v,envir)
  }
}


