  # A basic sample
  
  fixed.amount <- 1000
  fixed.interest.rate <- 0.05
  
  present.value <- Perpetuity.PresentValue(fixed.interest.rate = fixed.interest.rate,
                                        fixed.amount = fixed.amount)
  
  print(paste0("A perpetuity with an interest rate of ",
               fixed.interest.rate * 100,
               "% and paying a fixed amount of ",
               fixed.amount,
               " has a present value of ",
               present.value))