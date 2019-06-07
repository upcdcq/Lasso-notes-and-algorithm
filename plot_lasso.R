plot_lasso <-function(lambda,object){
  p = dim(object)[2]
  N = length(lambda)
  x_lim = c(min(lambda),max(lambda))
  y_lim = c(min(object),max(object))
  plot(lambda,object[,1],type = 'l',col = 1,
       xlim = x_lim,ylim = y_lim,
       main = "Profile of LASSO",
       xlab = 'log(lambda)')
  for (i in 2:p) {
    lines(lambda,object[,i],col = i+1,lty = 1,lwd = 3)
  }
}
