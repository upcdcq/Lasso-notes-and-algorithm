soft <- function(x,lambda) {
  if( x > 0.0 & lambda < abs(x)){
    return(x - lambda) 
  }
  else if(x < 0.0 & lambda < abs(x)){
    return( x + lambda)
  }
  else
    return(0.0)
}