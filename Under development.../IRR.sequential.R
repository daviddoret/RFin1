IRR.Sequential <- function(cashflow){
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
            is.numeric(cashflow))
  
  # special case: if cashflow is empty, IRR is 0
  # TODO(DD): Check if this is a standard approach
  if(length(cashflow) == 0){ return(0) }
  
  # special case: if cashflow contains only one item, IRR is 0
  # TODO(DD): Check if this is a standard approach
  if(length(cashflow) == 1){ return(0) }
  
  # TODO(DD): Make several algorithms available
  irr <- FunctionRoot.BisectionMethod(f = NPV.Sequential,
                                      cashflow = cashflow,
                                      endpoint.a = 1/1000000,
                                      endpoint.b = 1000000)
  
  return(irr)
}

IRR.Sequential.Sample1 <- function(){
  # A sample demonstration of the above function
  
  cashflow <- c(-100,0,20,-10,50,-5,80,90,200)
  print(paste("cashflow:",toString(cashflow)))
  irr <- IRR.Sequential(cashflow)
  print(paste0("irr: ", irr))
  npv <- NPV.Sequential(r = irr, cashflow = cashflow)
  print(paste0("npv based on the irr: ",npv))
  
}