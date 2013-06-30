IRR.sequential <- function(cashflow){
  # Computes the Internal Rate of Return of a cashflow.
  #
  # Status: BUGGED, TO BE REWRITTEN
  # 
  # Args:
  #   cashflow: a vector of cashflows
  #
  # Returns: 
  #   The IRR of the cashflow.
  #
  # References:
  #   Thanks to Stackoverflow community: http://stackoverflow.com/questions/11660187/any-r-package-available-to-calculate-irr-from-uneven-payments-on-specific-dates
  #
  stopifnot(is.vector(cashflow),
            is.numeric(cashflow),
            require(uniroot))
  
  # special case: if cashflow is empty, IRR is 0
  # TODO(DD): Check if this is a standard approach
  if(length(cashflow) == 0){ return(0) }
  
  # special case: if cashflow contains only one item, IRR is 0
  # TODO(DD): Check if this is a standard approach
  if(length(cashflow) == 1){ return(0) }
  
  irr <- uniroot(f = NPV.sequential, 
                 c(-1000,+1000),
                 cashflow = cashflow)$root
  
  return(irr)
}