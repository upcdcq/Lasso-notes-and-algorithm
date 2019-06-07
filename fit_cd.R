fit_cd = function(x_new, bb){
  return(X_new%*%bb[-1] + bb[1])
}