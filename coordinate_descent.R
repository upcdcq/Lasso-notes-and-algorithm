ls_cd = function(X,Y,lambda,iter = 1000,eps = 0.05){
  ##coordinate decent for lasso
  ##beta_lasso = argmin  RSS/(2 N) + lambda |beta|_1
  X= as.matrix(X)
  Y = as.matrix(Y)
  N = nrow(X); p =ncol(X)
  #if(!b_initial){b = rnorm(p)}
  z_k = apply(X, 2, function(x) sum(x^2)) / N
  b = rnorm(p)
  y_mean = mean(Y)
  X_mean = apply(X, 2, mean) 
  b0 =(y_mean  - X_mean%*%b)[1]
  Found = FALSE
  while (iter & !Found) {
    b_old = b
    for (j in 1:p) {
      b[j] = 0
      r = X[,j]%*%(Y - b0 - X%*%as.matrix(b,p,1)) / N
      b[j] = soft(r,lambda) / z_k[j]
    }
    if(sqrt(sum((b_old - b)^2))  < eps){
      Found = TRUE
    }
    b0 =(y_mean  - X_mean%*%b)[1]
    iter = iter - 1
  }
  b0 =y_mean  - X_mean%*%b
  result = c(b0,b)
  return(result)
}

# iter = 100
# while (iter & !Found) {
#   if(iter < 10){
#     Found = T
#   }
#   iter = iter - 1
# }