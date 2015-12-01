library(ggplot2)
data(diamonds)

dim(diamonds)
names(diamonds$color)
lapply(diamonds, is.ordered)
levels(diamonds$color)

qplot(diamonds$price, data=diamonds)

qplot(x = price, data = diamonds, binwidth = 25) +
  scale_x_continuous(limits = c(0,1500), breaks = seq(0, 1400, 100))

qplot(x = price, data = diamonds) +
  facet_wrap(~cut) +
  scale_x_continuous(limits = c(15000, 20000))

subset(diamonds, diamonds$price == max(diamonds$price))

subset(diamonds, diamonds$price == min(diamonds$price))

qplot(x = price, data = diamonds, binwidth = 25) + 
  facet_wrap(~cut, scales="free_y")

qplot(x = price/carat, data = diamonds) +
  + facet_wrap(~cut)
  + scale_x_log10()

qplot(x = cut, y = price/carat, data = diamonds, geom="boxplot")

qplot(x = color, y = price/carat, data = diamonds, geom = "boxplot") 

by(diamonds$price, diamonds$color, summary)

qplot(x = carat, y = ..count.., data = diamonds, geom = 'freqpoly', binwidth = .01) +
  scale_x_continuous(breaks=seq(0, 10, .1))

