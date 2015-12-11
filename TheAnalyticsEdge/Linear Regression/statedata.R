data(state)

statedata <- cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
str(statedata)

library(ggplot2)

plot(statedata$x, statedata$y)
ggplot(aes(x = x, y = y), data = statedata) +
  geom_point()

tapply(statedata$HS.Grad, statedata$state.region, mean)

boxplot(statedata$Murder ~ statedata$state.region)
ggplot(aes(x = state.region, y = Murder), data = statedata) +
  geom_boxplot()

outlier <- max(subset(statedata, state.region == "Northeast")$Murder)
subset(statedata, Murder == outlier)

model1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(model1)

plot(statedata$Income, statedata$Life.Exp)

model2 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(model2)

model3 <- lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(model3)

model4 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(model4)

sort(predict(model4))
statedata$state.abb[which.min(statedata$Life.Exp)]

sort(predict(model4))
statedata$state.abb[which.max(statedata$Life.Exp)]

sort(predict(model4) - statedata$Life.Exp)
sort(abs(model4$residuals))
