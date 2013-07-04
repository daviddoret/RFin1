NPV.GrowingPerpetuity <- function(r,x,g){
  # Computes the net present value (NPV) of a growing perpetuity.
  # 
  # Args:
  #   r: the interest rate, e.g.: 0.05 for 5%
  #   x: fixed amount payment
  #   g: constant growing rate
  # 
  # Returns: 
  #   The Net Present Value of the growing perpetuity.
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard  

  stopifnot(is.numeric(r), 
            length(r) == 1,
            is.numeric(x), 
            length(x) == 1,
            is.numeric(g), 
            length(g) == 1)
  
  if(x == 0){
    return(0)
  }
  
  if(g >= r){
    warning("A growing perpetuity with a growing rate that is equal to or higher than the interest rate has an infinite value")
    return(Inf)
  }
  
  return(x / (r - g))
}