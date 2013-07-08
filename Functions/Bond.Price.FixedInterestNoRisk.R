Bond.Price.FixedInterestNoRisk <- function(coupon.rate,
                                           face.value,
                                           periods.to.maturity){
  # Computes the price of a bond
  # 
  # Args:
  #   coupon.rate         : the rate (percentage) of the coupon
  #   face.value          : the face value paid at maturity
  #   periods.to.maturity : the number of periods to reach maturity
  # 
  # Returns: 
  #   The price of the bond
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard  
  
  stopifnot(is.numeric(coupon.rate), 
            length(coupon.rate) == 1,
            is.numeric(face.value), 
            length(face.value) == 1,
            is.numeric(periods.to.maturity), 
            length(periods.to.maturity) == 1)
  
  # If the face value is 0, it is pretty obvious that the price is 0
  if(face.value == 0){
    return(0)
  }
  
  # The price of a bond may be simply calculated as the present value of its cashflows.
  cashflow.sequence <- Bond.CashflowSequence(coupon.rate = coupon.rate,
                                            face.value = face.value,
                                            periods.to.maturity = periods.to.maturity)
  
  print(cashflow.sequence)
  
  return(CashflowSequence.PresentValue(fixed.interest.rate = coupon.rate,
                                       cashflow.sequence = cashflow.sequence))
}
