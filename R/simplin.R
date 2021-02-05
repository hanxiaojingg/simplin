simp_lin_R <- function(x,y){
  if(is.numeric(x)*is.numeric(y)*is.vector(x)*is.vector(y)<1){
    stop('should be numeric vector')
  }
  if((length(x)-length(y))!=0){
    stop('should have rhe asme length')
  }
  ans <- simp_lin_cpp(x,y,length(x))
  coef <- matrix(c(ans[[1]],ans[[2]]),2,4,byrow=TRUE)
  rownames(coef) <- c('beta0','beta1')
  colnames(coef) <- c('estimate','se','lower 95CI','upper 95CI')
  return(list(coef=coef,pred=ans[[3]],res=ans[[4]]))
}
