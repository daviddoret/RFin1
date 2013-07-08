FunctionRoot.BisectionMethod <- function(f,
                                         ...,
                                         max.iterations = 100,
                                         endpoint.a = -1000000,
                                         endpoint.b = +1000000,
                                         tolerance = 0.0000001){
  # Tries to find the root of a function (0)..
  # 
  # Args:
  #   f:              the function for which you would like to find the root
  #   max.iterations: the maximum number of iterations to avoid infinite loop.
  #                   usually, 50 is a safe value for most common usages.
  #   endpoint.a:     the algorithm only functions properly when you feed it with
  #                   two boundary values, of which one must yield a negative f(x)
  #                   and the other must yield a positive f(x).
  #   endpoint.b:     see endpoint.a.
  #   tolerance:      warning: this is not the precision of the result but the
  #                   size of the range within which the attempt to find the root
  #                   is made.
  #
  # Returns: 
  #   The root of the function, or at least a best effort estimate of the root of the function.
  # 
  # References:
  # - http://en.wikipedia.org/wiki/Bisection_method
  
  # Type checking
  stopifnot(is.numeric(max.iterations), 
            length(max.iterations) == 1,
            is.numeric(endpoint.a), 
            length(endpoint.a) == 1,
            is.numeric(endpoint.b), 
            length(endpoint.b) == 1,
            is.numeric(tolerance), 
            length(tolerance) == 1)
  
  # Pre-conditions for this algorithm
  stopifnot(endpoint.a < endpoint.b,
            (f(endpoint.a,...) < 0 & f(endpoint.b,...) > 0) | 
              (f(endpoint.a,...) > 0 & f(endpoint.b,...) < 0))
  
  a <- endpoint.a
  b <- endpoint.b
  n <- 1
  
  while (n < max.iterations){
    # Takes the mid-point between a and b
    c <- (a + b) / 2
    if(f(c,...) == 0 | (b - a) / 2 < tolerance){
      # We found a satisfying solution
      return(c)
    }
    n <- n + 1
    if(sign(f(c,...)) == sign(f(a,...))){
      a <- c
    }
    else
    {
      b <- c
    }
  }
  
  warning("The bisection method with max.iterations of ",
          max.iterations,
          " did not find a function root within the desired tolerance of ",
          tolerance,
          ". Its result was x=",
          c,
          " for f(x)=",
          f(c,...),
          ". The function nevertheless returned this result.",
          " You should try with a higher max.iterations or a ",
          "different root finding algorithm")
  
  return(c)
  
}

FunctionRoot.BisectionMethod.Sample1 <- function(){
  # A sample usage of the function
  
  f <- function(x){
    return( 2 * x ^ 2 - 500 )
  }

  x <- FunctionRoot.BisectionMethod(f)
  fx <- f(x)
  print(f)
  print(paste("x: ",x))
  print(paste("f(x): ",fx))
  
}