who <- read.csv("WHO.csv")

plot(who$GNI, who$FertilityRate)

library(ggplot2)

scatterplot <- ggplot(aes(x = GNI, y= FertilityRate), data=who)
fertilityGNIplot <- scatterplot + geom_point(color="darkred", size=3, shape=8) + ggtitle("Fertility Rate vs Gross National Income")
scatterplot + geom_line()

scatterplot + geom_point(shape=15)

pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

ggplot(aes(x = GNI, y = FertilityRate, color = Region), data = who) + geom_point()
ggplot(aes(x = GNI, y = FertilityRate, color = LifeExpectancy), data = who) + geom_point()

ggplot(aes(x = log(FertilityRate), y = Under15), data = who) + geom_point()
model <- lm(Under15 ~ log(FertilityRate), data=who)
summary(model)
ggplot(aes(x = log(FertilityRate), y = Under15), data = who) + geom_point() +
  stat_smooth(method="lm", level=0.99)
ggplot(aes(x = log(FertilityRate), y = Under15), data = who) + geom_point() +
  stat_smooth(method="lm", se=FALSE, color="orange")
ggplot(aes(x=log(FertilityRate), y=Under15, color=Region), data=who) + geom_point(size=5)
#color blind palette
#scale_color_brewer(palette="Dark2")