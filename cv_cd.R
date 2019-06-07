cv_cd = function(X,Y,Lambda,K = 10){
  N = length(Y)
  len = length(Lambda)
  s = sample(1:N,N)
  Y = Y[s]
  X = X[s,]
  cv_error = matrix(0,K,len)
  for (i in 1:K) {
    begin = ceiling((i-1)*N/K+1)
    over = min(ceiling(i*N/K) ,N)
    for (j in 1:len) {
      fit = ls_cd(X[-(begin:over),],Y[-(begin:over)],Lambda[j])
      cv_error[i,j] = loss(X[(begin:over),],Y[(begin:over)],
                           fit)
    }
  }
  error_mean = apply(cv_error, 2, mean)
  error_lb= error_mean - apply(cv_error, 2, sd)
  error_ub = error_mean + apply(cv_error, 2, sd)
  
  return(data.frame(error_mean,error_lb,error_ub))
}