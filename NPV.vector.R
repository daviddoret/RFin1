NPV.vector <- function(r,cashflow){
  # cashflow: a vector of sequential cashflows, c0, c1, c2, ..., cn
  
  # TODO(DMD): Check the data type of the cashflow vector
  # TODO(DMD): Check that r is numeric 
  
  # special case: if cashflow is empty, value is 0
  if(length(cashflow) == 0){ return(0) }
  
  # special case: if cashflow contains only one item, value is equal to that item
  if(length(cashflow) == 1){ return(cashflow[1])}
  
  periods <- seq(from = 0, to = length(cashflow) - 1, by = 1)
  factors <- (1 + r) ^ periods
  pv <- cashflow / factors
  
  return (sum(pv))
}