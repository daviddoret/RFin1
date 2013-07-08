# This script uses the perpetuity functions in order to illustrate with a graph
# the effect of various discount rates (or interest rates) on present value.

require(lubridate)
require(ggplot2)

# Builds a data frame with observations for various combinations of 
# discount rate, amount paid.
fixed.interest.rate <- seq(from = 0.02, to = 1, by = 0.02)
fixed.amount <- c(100)
data <- expand.grid(fixed.interest.rate,fixed.amount)
colnames(data) <- c("fixed.interest.rate","fixed.amount")
data$label <- paste0("Payment of a fixed amount of ", data$fixed.amount," with a ", data$fixed.interest.rate * 100,"% discount rate")
data$present.value <- Perpetuity.PresentValue(data = data)

attach(data)

chart <- ggplot(data = data) + 
  theme(legend.position = "bottom") +
  xlab("Discount rate") +
  ylab("Present value") +
  ggtitle(paste0("THE EFFECT OF DISCOUNT RATE ON THE PRESENT VALUE OF PERPETUITIES")) +
  geom_bar(stat = "identity",
    aes(x = fixed.interest.rate,
        y = present.value))

plot(chart) 



