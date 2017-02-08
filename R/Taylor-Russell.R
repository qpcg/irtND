#' Taylor Russell Tables
#'
#' Taylor Russell tables estimate the percentage of new hires that will be successful if a particular selection method is used.
#' @param rho A vector of validity values for an assessment
#' @param base A vector of base rates of potential hires in the population that would be successful
#' @param select A vector of selction ratios: i.e. the top x% of employees to be selected.
#' @keywords Taylor Russell
#' TaylorRussell()

TaylorRussell <- function(rho,base,select){
my.array<-array(0,dim=c(length(rho),length(select),length(base)))
for(i in 1: length(rho)){
for(j in 1:length(select)){
for(k in 1:length(base)){
#corr matrix  
corr <- matrix(c(1,rho[i],rho[i],1),nrow=2,ncol=2,byrow=T)


top <- pmvnorm(lower=c(qnorm(select[j], lower.tail = FALSE),qnorm(base[k], lower.tail = FALSE)), upper=c(Inf,Inf), mean=rep(0, 2),
        corr, algorithm = GenzBretz())

bottom <- pmvnorm(lower=c(qnorm(select[j], lower.tail = FALSE),-Inf), upper=c(Inf,Inf), mean=rep(0, 2),
                corr, algorithm = GenzBretz())

my.array[i,j,k]<-top[1]/bottom[1]
}
}
}  
return(my.array)
}


