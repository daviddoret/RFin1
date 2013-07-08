FV.Sequential <- function(r,cashflow){
  # Computes the Future Value (FV) of a cashflow.
  # 
  # Args:
  #   r:        the interest rate
  #   cashflow: a vector of cashflows
  #
  # Returns: 
  #   The Future Value of the cashflow.
  #
  stopifnot(is.numeric(r), 
            length(r) == 1,
            is.vector(cashflow),
            is.numeric(cashflow))
  
  # special case: if cashflow is empty, value is 0
  if(length(cashflow) == 0){ return(0) }
  
  # special case: if cashflow contains only one item, value is equal to that item
  if(length(cashflow) == 1){ return(cashflow[1])}
  
  periods <- seq(from = length(cashflow) - 1, to = 0, by = -1)
  factors <- (1 + r) ^ periods
  pv <- cashflow * factors
  
  return (sum(pv))
}