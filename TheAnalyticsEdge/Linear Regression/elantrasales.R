elantra <- read.csv("elantra.csv")
str(elantra)

elantra_train <- subset(elantra, Year <= 2012)
elantra_test <- subset(elantra, Year > 2012)

model1 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantra_train)
summary(model1)

model2 <- lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = elantra_train)
summary(model2)

elantra_train$Month_factor <- as.factor(elantra_train$Month)
elantra_test$Month_factor <- as.factor(elantra_test$Month)
model3 <- lm(ElantraSales ~ Month_factor + Unemployment + CPI_all + CPI_energy + Queries, data = elantra_train)
summary(model3)

cor(elantra_train[c("Unemployment", "Month", "Queries", "CPI_energy", "CPI_all")])

model4 <- lm(ElantraSales ~ Month_factor + Unemployment + CPI_all + CPI_energy, data = elantra_train)
summary(model4)

prediction <- predict(model4, newdata = elantra_test)
SSE <- sum((prediction - elantra_test$ElantraSales)^2)
baseline_prediction <- mean(elantra_train$ElantraSales)
baseline_prediction
SST <- sum((elantra_test$ElantraSales - baseline_prediction)^2)
SST
which.max(abs(prediction - elantra_test$ElantraSales))
#returns ID 14 and row 5, confusing at first...