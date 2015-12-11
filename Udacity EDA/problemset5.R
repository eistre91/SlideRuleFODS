library(ggplot2)
library(dplyr)
data(diamonds)

qplot(x = price, data = diamonds, color = cut) +
  facet_wrap(~color) +
  scale_fill_brewer(type = 'qual')

ggplot(aes(x = price), data = diamonds) +
  geom_histogram(aes(fill = cut)) +
  facet_wrap(~color)

ggplot(aes(x = table, y = price), data = diamonds) +
  geom_point(aes(color = cut)) +
  scale_x_continuous(limits = c(43, 95), breaks = seq(43, 95, 2))

diamonds <- diamonds %>%
  mutate(volume = x * y * z)

ggplot(aes(x = volume, y = price), data = subset(diamonds, volume > 0)) +
  geom_point(aes(color = clarity)) +
  scale_x_continuous(limits = c(0, quantile(diamonds$volume, .99)))

pf <- read.delim("pseudo_facebook.tsv")

pf <- pf %>%
  mutate(prop_initiated = friend_count/friendships_initiated)

pf <- transform(pf, prop_initiated = friendships_initiated/friend_count)

pf <- transform(pf, prop_initiated = ifelse(friend_count > 0, friendships_initiated/friend_count, NA))

head(pf %>% filter(!is.na(prop_initiated)))
pf %>% filter(!is.na(prop_initiated), prop_initiated > 0, prop_initated < 1)

pf$year_joined <- floor(2014 - pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))

ggplot(aes(x = tenure, y = prop_initiated), data = pf) +
  geom_line(aes(color=year_joined.bucket))

ggplot(aes(x = 20 * round(tenure / 20), y = prop_initiated), data = filter(pf, prop_initiated > 0, prop_initiated < 1)) +
  geom_line(aes(color=year_joined.bucket))

filter(pf, prop_initiated > 0, prop_initiated < 1)
range(pf$prop_initated)

ggplot(aes(x = cut, y = price/carat), data = diamonds) +
  geom_jitter(aes(color = color)) +
  facet_wrap( ~ clarity)
