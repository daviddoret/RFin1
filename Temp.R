# A bunch of tests, ideas and code snippets

require(googleVis)

#chart <- gvisMotionChart(data=data,
#                         idvar = "idvar",
#                         timevar = "timevar",
#                         xvar = "xvar",
#                         yvar = "yvar",
#                         sizevar = "sizevar")
#  
#  plot(chart)

#chart <- gvisMotionChart(data=data)


#plot(chart)

# ANOTHER POSSIBLE APPROACH

#if (!require("gWidgetsRGtk2")) install.packages("gWidgetsRGtk2")
#library(gWidgetsRGtk2)
#options(guiToolkit = "RGtk2")
#graphics.off()
#x11()
#x11()
#dev.set()

#gslider(from = 1, to = 100, value = 10,
#        container = gwindow("Two Plotting Windows"),
#        handler = function(h, ...) {
#          n = 10 * svalue(h$obj)
#          plot(rnorm(n), main = "the 1st plot")
#          dev.set()
#          plot(hist(runif(n)), main = "the 2nd plot")
#          dev.set()
#        })
#}