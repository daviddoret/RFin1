Bond.Cashflow <- function(t,c,f){
  # Returns the cashflow of a bond
  # If you want to compute the present value of the bond, use the NPV function
  # 
  # Args:
  #   t: the number of periods (years or other frequency)
  #   c: the coupon
  #   f: the face value
  #
  # Returns: 
  #   A vector containing the cashflow of a bond.
  #   This cashflow is composed like this:
  #        Period:         0     1     2     3     ...     t
  #        Coupon:         c     c     c     c     ...     c
  #        Face value:                                     f
  #   The cashflow will contain t + 1 elements, because it will contain
  #   a cashflow of 0 at t = 0.
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard
  
  stopifnot(is.numeric(t), 
            length(t) == 1,
            t > 0,
            is.numeric(c), 
            length(c) == 1,
            is.numeric(f), 
            length(f) == 1)

  return(c(
    0,
    rep(x = c, times = t - 1),
    c + f))

}
