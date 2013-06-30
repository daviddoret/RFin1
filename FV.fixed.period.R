FV.fixed.period <- function(r,amount,n){
  # Computes the Future Value (FV) of a cashflow with a fixed number of periods.
  # 
  # Args:
  #   r:        the interest rate
  #   amount:   an amount (single numeric value or vector of numerics)
  #             if a vector is passed, please note that a single numeric
  #             value will be returned, corresponding to the total future value.
  #   n:        the number of periods
  # 
  # Returns: 
  #   The Future Value of the cashflow,
  #   In the form of a single numeric value.
  #
  stopifnot(is.numeric(r), 
            length(r) == 1,
            is.vector(cashflow),
            is.numeric(cashflow),
            is.numeric(n),
            length(n)==1,
            n >= 0) 

  factor <- (1 + r) ^ n
  pv <- sum(amount * factor)
  
  return(pv)
}