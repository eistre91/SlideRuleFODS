fram <- read.csv("framingham.csv")
str(fram)

library(caTools)
set.seed(1000)
split <- sample.split(fram$TenYearCHD, SplitRatio = 0.65)

fram_train <- subset(fram, split)
fram_test <- subset(fram, !split)

fram_log <- glm(TenYearCHD ~ ., data = fram_train, family=binomial)
summary(fram_log)

predict_test <- predict(fram_log, type = "response", newdata = fram_test)
table(fram_test$TenYearCHD, predict_test > 0.5)
#model accuracy
(1069 + 11) / (1069+6+187+11)
#baseline model
(1069 + 6) / (1069+6+187+11)

library(ROCR)
ROCR_pred <- prediction(predict_test, fram_test$TenYearCHD)
as.numeric(performance(ROCR_pred, "auc")@y.values)

