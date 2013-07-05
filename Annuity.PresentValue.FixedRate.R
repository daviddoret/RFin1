Annuity.PresentValue.FixedRate <- function(fixed.interest.rate,
                                           fixed.amount,
                                           periods.number){
  # Computes the Net Present Value (NPV) of an annuity.
  # 
  # Args:
  #   fixed.interest.rate: the interest rate, e.g.: 0.05 for 5%
  #   fixed.amount: cashflow, fixed amount payment
  #   periods.number: the number of periods
  # 
  # Returns: 
  #   The Net Present Value of the annuity.
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard  
  
  stopifnot(is.numeric(fixed.interest.rate), 
            length(fixed.interest.rate) == 1,
            is.numeric(fixed.amount), 
            length(fixed.amount) == 1,
            is.numeric(periods.number), 
            length(periods.number) == 1,
            periods.number >= 0)
  
  # If the cashflow value is 0, it is pretty obvious that the PV is 0
  if(fixed.amount == 0){
    return(0)
  }
  
  # If there are no payment periods, there are no payments
  if(periods.number == 0){
    return(0)
  }
  
  # I do not use the simplified formula
  # in such a way as to avoid a division by 0
  # if fixed.interest.rate == 0.
  
  return(sum(rep(x = fixed.amount, times = periods.number) / 
    rep(x = (1 + fixed.interest.rate),times = periods.number) ^ seq(1,periods.number)))
}
