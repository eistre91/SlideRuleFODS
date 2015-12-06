library(ggplot2)
library(dplyr)
data(diamonds)

dim(diamonds)

ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point()

cor.test(x = diamonds$x, y = diamonds$price)
cor.test(x = diamonds$y, y = diamonds$price)
cor.test(x = diamonds$z, y = diamonds$price)

head(diamonds)

ggplot(aes(x = depth, y = price), data = diamonds) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(43, 79, 2))

cor.test(x = diamonds$depth, y = diamonds$price)

ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point() + 
  scale_x_continuous( limits = c(0, quantile(diamonds$carat, .99))) +
  scale_y_continuous( limits = c(0, quantile(diamonds$price, .99)))

ggplot(aes(x = x * y * z, y = price), data = diamonds) +
  geom_point() +
  scale_x_continuous(limits = c(0, quantile(diamonds$volume, .99)))

?mutate
diamonds <- mutate(diamonds, volume = x * y * z)

reasonable_diamonds <- subset(diamonds, volume > 0 & volume < 800)
cor.test( x = reasonable_diamonds$price, y = reasonable_diamonds$volume)

ggplot(aes(x = volume, y = price), data = reasonable_diamonds) +
  geom_point(alpha = 1/100) +
  geom_smooth(method = "lm")

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) 

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

diamonds_by_cut <- group_by(diamonds, cut)
diamonds_mp_by_cut <- summarise(diamonds_by_cut, mean_price = mean(price))

ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity")

ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")


ggplot(aes(x = cut, y = mean_price), data = diamonds_mp_by_cut) +
  geom_bar(stat = "identity")
