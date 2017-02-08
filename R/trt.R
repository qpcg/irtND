#' Test Retest Matrix
#'
#' Creates a matrix of correlations with inter-item correlations from time 1 and time 2 on the respective off diagonals with test retest reliability on the main diagonal. Useful in conjunction with corrplot().
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' TestRetest()


TestRetest <- function(dat1, dat2, use=c("everything","all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")){
  new.mat <- cor(dat1,use = use)*lower.tri(cor(dat1,use = use)) + cor(dat2,use = use)*upper.tri(cor(dat2,use = use))
  diag(new.mat) <- diag(cor(dat1,dat2,use = use))
  return(new.mat)}






