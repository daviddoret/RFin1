Perpetuity.PresentValue.FixedRate <- function(fixed.interest.rate,
                                           fixed.amount){
  # Computes the present value (PV) of a perpetuity.
  # 
  # Args:
  #   fixed.interest.rate: the interest rate, e.g.: 0.05 for 5%
  #   fixed.amount: cashflow, fixed amount payment
  # 
  # Returns: 
  #   The present value (PV)
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard  
  
  stopifnot(is.numeric(fixed.interest.rate), 
            length(fixed.interest.rate) == 1,
            is.numeric(fixed.amount), 
            length(fixed.amount) == 1)
  
  # If the cashflow value is 0, it is pretty obvious that the PV is 0
  if(fixed.amount == 0){
    return(0)
  }
  
  if(fixed.interest.rate <= 0)
  {
    # If the discount rate is 0 or less,
    # there is no discounting over time.
    # If there is no discounting over time,
    # value converges towards infinity.
    return(Inf)
  }
  
  return(fixed.amount / fixed.interest.rate)
}
