#' Calculate Polycoric Correlation Matrices using multiple cores
#'
#' This function estimates a pairwise polychoric correlation matrix using the mclapply function in the paralell packages. Note windows machines are unable to do paralell processing using forking. See package snow for more options.
#' @param dat A dataset of ordered categorical data
#' @param n.cores The number of cores to use. See function detectCores(). default = 1.
#' @keywords Polychoric Correlation Matrix
#' Polycor.matrix()

Polycor.matrix <- function(dat,n.cores = 1){
  nrow <- ncol(dat)
  out <- mclapply(data.frame(combn(ncol(dat),2)), function(x){
    cases <- complete.cases(dat[,c(x[1],x[2])])
    pc <- polychor(table(dat[cases,x[1]],dat[cases,x[2]]))
    pc
  },mc.cores = n.cores)
  sim.mat <- diag(x=1,nrow = nrow)
  l <- length(out)
  x <- combn(ncol(dat),2)
  for(i in 1:l){
    sim.mat[x[1,i],x[2,i]] <- out[[i]]
  }
  sim.mat[lower.tri(sim.mat)] <- t(sim.mat)[lower.tri(sim.mat)]
  return(sim.mat)
}







