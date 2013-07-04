NPV.Annuity <- function(r,x,t){
  # Computes the Net Present Value (NPV) of an annuity.
  # 
  # Args:
  #   r: the interest rate, e.g.: 0.05 for 5%
  #   x: fixed amount payment
  #   t: the number of periods
  # 
  # Returns: 
  #   The Net Present Value of the annuity.
  #
  # Credits & references:
  #  - Financian Numerical Recipes in C++, January 2006, Bernt Arne 0degaard  
  
  stopifnot(is.numeric(r), 
            length(r) == 1,
            is.numeric(x), 
            length(x) == 1,
            is.numeric(t), 
            length(t) == 1,
            t >= 0)
  
  if(x == 0){
    return(0)
  }
  
  if(t == 0){
    return(0)
  }
  
  # I do not use the simplified formula
  # in such a way as to avoid a division by 0
  # if r == 0.
  
  return(sum(rep(x = x, times = t) / 
    rep(x = (1 + r),times = t) ^ seq(1,t)))
}

NPV.Annuity.Sample = function(){

  x <- 1000
  t <- 20
  r <- 0.05
  print(paste0("An annuity with ",
               t,
               " periods, an interest rate of ",
               r * 100,
               "%, paying a fixed amount of ",
               x,
               " has a net present value of ",
               NPV.Annuity(r=r,x=x,t=t)))

  x <- 900
  t <- 20
  r <- 0.05
  print(paste0("An annuity with ",
               t,
               " periods, an interest rate of ",
               r * 100,
               "%, paying a fixed amount of ",
               x,
               " has a net present value of ",
               NPV.Annuity(r=r,x=x,t=t)))

  x <- 1000
  t <- 18
  r <- 0.05
  print(paste0("An annuity with ",
               t,
               " periods, an interest rate of ",
               r * 100,
               "%, paying a fixed amount of ",
               x,
               " has a net present value of ",
               NPV.Annuity(r=r,x=x,t=t)))

  x <- 1000
  t <- 20
  r <- 0.07
  print(paste0("An annuity with ",
               t,
               " periods, an interest rate of ",
               r * 100,
               "%, paying a fixed amount of ",
               x,
               " has a net present value of ",
               NPV.Annuity(r=r,x=x,t=t)))
}