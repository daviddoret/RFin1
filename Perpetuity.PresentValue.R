Perpetuity.PresentValue <- function(...){
  # Computes the present value (PV) of a perpetuity.
  #
  # Syntax #1
  #
  # Args:
  #   fixed.interest.rate: (numeric) the interest rate, e.g.: 0.05 for 5%
  #   fixed.amount: (numeric) cashflow, fixed amount payment
  #
  # Returns: 
  #   The present value of the perpetuity.
  #
  # Syntax #2
  #
  # Args:
  #   data : a data frame with the following columns:
  #          fixed.interest.rate (numeric)
  #          fixed.amount (numeric)
  #
  # Returns:
  #   A vector whose length is equal to the number of rows in the data frame,
  #   containing the present values of every individual row.
  
  args <- list(...)
  
  if(all(names(args) == c("fixed.interest.rate","fixed.amount"))){
    return(Perpetuity.PresentValue.FixedRate(fixed.interest.rate = args$fixed.interest.rate, 
                                          fixed.amount = args$fixed.amount))
  }
  if(all(names(args) == c("data"))){
    mapply(FUN=Perpetuity.PresentValue.FixedRate,
           fixed.interest.rate = args$data$fixed.interest.rate,
           fixed.amount = args$data$fixed.amount)
  }
}