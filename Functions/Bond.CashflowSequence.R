Bond.CashflowSequence <- function(coupon.rate,
                                  face.value,
                                  periods.to.maturity)
{
  # Returns the cashflow of a bond
  # If you want to compute the present value of the bond, use the NPV function
  # 
  # Args:
  #   coupon.rate         : the rate (percentage) of the coupon
  #   face.value          : the face value paid at maturity
  #   periods.to.maturity : the number of periods to reach maturity
  #
  # Returns: 
  #   A vector containing the cashflow of a bond.
  #   This cashflow is composed like this:
  #        Period:         0     1     2     3     ...     periods.to.maturity
  #        Coupon:         c     c     c     c     ...     coupon.rate * face.value
  #        Face value:                                     face.value
  #   The cashflow will contain periods.to.maturity + 1 elements, because it will contain
  #   a cashflow of 0 at t = 0.
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard
  
  stopifnot(is.numeric(periods.to.maturity), 
            length(periods.to.maturity) == 1,
            periods.to.maturity > 0,
            is.numeric(coupon.rate), 
            length(coupon.rate) == 1,
            is.numeric(face.value), 
            length(face.value) == 1)

  return(c(
    0,
    rep(x = coupon.rate * face.value, 
        times = periods.to.maturity - 1),
        (1 + coupon.rate) * face.value))

}
