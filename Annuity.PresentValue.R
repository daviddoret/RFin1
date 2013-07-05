Annuity.PresentValue <- function(...){
  # Computes the present value (PV) of an annuity.
  # 
  # Args:
  #   Flexible arguments, see underlying functions
  # 
  # Returns: 
  #   The present value of the annuity.
  #
  
  args <- list(...)
  
  if(all(names(args) == c("fixed.interest.rate","fixed.amount","periods.number"))){
    return(Annuity.PresentValue.FixedRate(fixed.interest.rate = args$fixed.interest.rate, 
                                          fixed.amount = args$fixed.amount,
                                          periods.number = args$periods.number))
  }
  if(all(names(args) == c("data"))){
    mapply(FUN=Annuity.PresentValue.FixedRate,
           fixed.interest.rate = args$data$fixed.interest.rate,
           fixed.amount = args$data$fixed.amount,
           periods.number = args$data$periods.number)
  }
}

Annuity.PresentValue.Sample = function(){
  # A basic sample
  fixed.amount <- 1000
  periods.number <- 20
  fixed.interest.rate <- 0.05
  print(paste0("An annuity with ",
               periods.number,
               " periods, an interest rate of ",
               fixed.interest.rate * 100,
               "%, paying a fixed amount of ",
               fixed.amount,
               " has a present value of ",
               Annuity.PresentValue(fixed.interest.rate = fixed.interest.rate,
                                    fixed.amount = fixed.amount,
                                    periods.number = periods.number)))
}

Annuity.PresentValue.Demo = function(){
  
  rs <- c(seq(from = 0.01,
              to = 0.20,
              by = 0.01))
  
  cs <- c(seq(from = 1,
              to = 100,
              by = 20))
  
  ts <- c(seq(from = 1,
              to = 50,
              by = 10))
  
  data <- expand.grid(rs,cs,ts)
  colnames(data) <- c("fixed.interest.rate","fixed.amount","periods.number")
  
  data$present.value <- Annuity.PresentValue(data = data)
  data$id <- seq(from = 1, to = nrow(data), by = 1)
  
  head(data)
  
  #return(data)
  require(googleVis)
  
  chart <- gvisMotionChart(data=data,
                   idvar = "id",
                   timevar = "periods.number",
                   xvar = "fixed.interest.rate",
                   yvar = "present.value",
                   colorvar = "fixed.amount",
                   sizevar = "fixed.amount")

  plot(chart)
  
  # ANOTHER POSSIBLE APPROACH
  
  if (!require("gWidgetsRGtk2")) install.packages("gWidgetsRGtk2")
  library(gWidgetsRGtk2)
  options(guiToolkit = "RGtk2")
  graphics.off()
  x11()
  x11()
  dev.set()
  
  gslider(from = 1, to = 100, value = 10,
          container = gwindow("Two Plotting Windows"),
          handler = function(h, ...) {
            n = 10 * svalue(h$obj)
            plot(rnorm(n), main = "the 1st plot")
            dev.set()
            plot(hist(runif(n)), main = "the 2nd plot")
            dev.set()
          })
}