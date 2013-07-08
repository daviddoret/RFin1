CashflowSequence.PresentValue <- function(fixed.interest.rate,
                                           cashflow.sequence){
  # Computes the present value (PV) of cashflow sequence
  # 
  # Args:
  #   fixed.interest.rate : the interest rate, e.g.: 0.05 for 5%
  #   cashflow.sequence   : a vector of cashflows, including time 0.
  # 
  # Returns: 
  #   The present value (PV)
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard  
  
  stopifnot(is.numeric(fixed.interest.rate), 
            length(fixed.interest.rate) == 1,
            is.numeric(cashflow.sequence))
  
  # If the cashflow sequence is empty, it is pretty obvious that the PV is 0
  if(length(cashflow.sequence) == 0){
    return(0)
  }
  
  return(sum(cashflow.sequence /
    (1 + fixed.interest.rate) ^
    seq(from = 0, to = length(cashflow.sequence) - 1, by = 1)))
         
}
