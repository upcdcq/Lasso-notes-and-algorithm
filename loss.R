loss <-function(X,Y,Beta){
  N = length(Y)
  return(sum((Y - Beta[1] - X %*%Beta[-1])^2)/N )
  #+ lambda*norm(as.matrix(abs(Beta)))
}