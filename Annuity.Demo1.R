# This script uses the annuity functions in order to illustrate with a graph
# the effects of time, amount paid and discount rate on annuities.
  
require(lubridate)
require(ggplot2)

# Builds a data frame with observations for various combinations of 
# discount rate, amount paid and number of periods.
fixed.interest.rate <- c(seq(from = 0.01,
              to = 0.20,
              by = 0.01))
fixed.amount <- c(seq(from = 0,
              to = 100,
              by = 20))
periods.number <- c(seq(from = 1,
              to = 750,
              by = 1))
data <- expand.grid(fixed.interest.rate,fixed.amount,periods.number)
colnames(data) <- c("fixed.interest.rate","fixed.amount","periods.number")
data$present.value <- Annuity.PresentValue(data = data)
data$line <- paste0(data$fixed.amount," @ ", data$fixed.interest.rate * 100,"%")
attach(data)

chart <- ggplot(data = data) + 
  facet_grid(. ~ fixed.amount,
             labeller = function(variable,value){
             return (paste0("Amount paid: ",value))
             }) + 
  theme(legend.position = "bottom") +
  xlab("Time (i.e. number of periods)") +
  ylab("Present value") +
  scale_colour_continuous(name = "Discount rate") +
  ggtitle("An illustration of the effects of time, amount paid and discount rate on annuities") +
  geom_line(aes(x = periods.number,
                y = present.value, 
                group = line, 
                colour = fixed.interest.rate))

plot(chart) 




