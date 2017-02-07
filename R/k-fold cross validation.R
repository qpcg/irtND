#' K-fold Cross Validation
#'
#' This function splits the dataset into K-folds with a test set and a train set, returning the split datasets as a list.
#' @param dat The dataset to split
#' @param k The number of folds
#' Kfold()


Kfold <- function(dat,k){
ss<-sample(nrow(dat),nrow(dat))
folds <- cut(ss,breaks=k,labels=FALSE)

train<-test<-vector("list",length=k)    
for(i in 1:k){     
  w <- which(folds==i)
  test[[i]] <- dat[w, ]
  train[[i]] <- dat[-w, ]
}
return(list(test=test,train=train))
}

