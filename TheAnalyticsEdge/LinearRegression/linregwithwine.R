wine <- read.csv('wine.csv')
str(wine)

model1 <- lm(Price ~ AGST, data=wine)
summary(model1)

model1$residuals
SSE <- sum(model1$residuals^2)
SSE

model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)

SSE <- sum(model2$residuals^2)
SSE

model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)

SSE <- sum(model3$residuals^2)
SSE

model5 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)

SSE <- sum(model4$residuals^2)
SSE

model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)

cor(x = wine$WinterRain, y = wine$Price)
cor(x = wine$Age, y = wine$FrancePop)
cor(x = wine$HarvestRain, y = wine$WinterRain)
cor(wine)

model6 <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model6)

wine_test <- read.csv("wine_test.csv")
str(wine_test)
predict_test <- predict(model4, newdata=wine_test)
predict_test

SSE <- sum((wine_test$Price - predict_test)^2)
SST <- sum((wine_test$Price - mean(wine$Price)) ^ 2)
1-SSE/SST
