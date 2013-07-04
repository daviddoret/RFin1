NPV.Perpetuity <- function(r,x){
  # Computes the Net Present Value (NPV) of a perpetuity.
  # 
  # Args:
  #   r: the interest rate, e.g.: 0.05 for 5%
  #   x: fixed amount payment
  # 
  # Returns: 
  #   The Net Present Value of the perpetuity.
  #
  stopifnot(is.numeric(r), 
            length(r) == 1,
            is.numeric(x), 
            length(x) == 1)
  
  if(x == 0){
    return(0)
  }
  
  if(r == 0){
    warning("A perpetuity with a 0 interest rate has an infinite value.")
    return(Inf)
  }
  
  return (x / r)
}