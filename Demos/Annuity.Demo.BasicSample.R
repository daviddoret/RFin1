  # A basic sample
  
  fixed.amount <- 1000
  periods.number <- 20
  fixed.interest.rate <- 0.05
  
  present.value <- Annuity.PresentValue(fixed.interest.rate = fixed.interest.rate,
                                        fixed.amount = fixed.amount,
                                        periods.number = periods.number)
  
  print(paste0("An annuity with ",
               periods.number,
               " periods, an interest rate of ",
               fixed.interest.rate * 100,
               "% and paying a fixed amount of ",
               fixed.amount,
               " has a present value of ",
               present.value))