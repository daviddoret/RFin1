# This script uses the annuity functions in order to illustrate with a graph
# how discount rate and payment compete with each other.

require(lubridate)
require(ggplot2)

# Builds a data frame with observations for various combinations of 
# discount rate, amount paid and number of periods.
fixed.interest.rate <- c(0.01,0.05)
fixed.amount <- c(NA)
periods.number <- c(seq(from = 1,
                        to = 100,
                        by = 1))
data <- expand.grid(fixed.interest.rate,fixed.amount,periods.number)
colnames(data) <- c("fixed.interest.rate","fixed.amount","periods.number")
data[data$fixed.interest.rate == fixed.interest.rate[1],2] <- 100
data[data$fixed.interest.rate == fixed.interest.rate[2],2] <- 200
data$label <- paste0("Payment of ", data$fixed.amount," with ", data$fixed.interest.rate * 100,"% discount rate")
data$present.value <- Annuity.PresentValue(data = data)

attach(data)

chart <- ggplot(data = data) + 
  theme(legend.position = "bottom") +
  xlab("Time (i.e. number of periods)") +
  ylab("Present value") +
  scale_colour_discrete(name = "Annuity") +
  ggtitle(paste0("DISCOUNT RATE vs PAYMENT")) +
  geom_line(aes(x = periods.number,
                y = present.value, 
                group = label, 
                colour = label))

plot(chart) 



