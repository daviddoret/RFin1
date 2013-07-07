# This script uses the annuity functions in order to illustrate with a graph
# how time (i.e. number of periods) make the present value converges towards
# payment / discount rate
  
require(lubridate)
require(ggplot2)

# Builds a data frame with observations for various combinations of 
# discount rate, amount paid and number of periods.
fixed.interest.rate <- c(0.05)
fixed.amount <- c(100)
periods.number <- c(seq(from = 1,
              to = 1000,
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
  ggtitle(paste0("THE EFFECT OF TIME ON ANNUITIES",
                 "\n\nThis graph shows the present value as a function of time for an annuity that",
                 "\npays a fixed amount of 100 per period with a discount rate of 5%.",
                 "\n\nIt shows the effect of time on the annuity: we see how the present value",
                 "\nconverges towards: amount paid / discount rate.")) +
  geom_line(aes(x = periods.number,
                y = present.value, 
                group = line, 
                colour = fixed.interest.rate))

plot(chart) 




