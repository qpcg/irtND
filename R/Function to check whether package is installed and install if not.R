#' Load or Install then load a package
#'
#' Loads a library. In the library is not installed, the function will install then load the package.
#' @param pgk The
#' @keywords R packages, libraries
#' InstLoad()

InstLoad <- function(pkg){
  if(is.element(pkg, installed.packages()[,1]) == T){
    require(pkg,character.only = T)
  }else{
    install.packages(pkg)
    require(pkg,character.only = T)
  }
}
