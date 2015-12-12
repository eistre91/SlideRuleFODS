quality <- read.csv("quality.csv")
str(quality)

table(quality$PoorCare)

#baseline prediction is just that all patients
#receive good care
#gives accuracy of about 75%

install.packages("caTools")
library(caTools)

set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)

quality_train <- subset(quality, split)
quality_test <- subset(quality, !split)

nrow(quality_train)
nrow(quality_test)

quality_log <- glm(PoorCare ~ OfficeVisits + Narcotics, data=quality_train, family=binomial)
summary(quality_log)

predict_train <- predict(quality_log, type="response")
summary(predict_train)

#all the ones that actually received poor care have a higher
#mean probability that they received poor care
#in the logistic model
tapply(predict_train, quality_train$PoorCare, mean)

quality_log2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = quality_train, family = binomial)
summary(quality_log2)

#Confusion matrix
table(quality_train$PoorCare, predict_train > 0.5)
#sensitivity, true positive rate
10/25
#specificity, true negative rate
70/74

#Confusion matrix
table(quality_train$PoorCare, predict_train > 0.7)
#sensitivity, true positive rate
8/25
#specificity, true negative rate
73/74

#Confusion matrix
table(quality_train$PoorCare, predict_train > 0.2)
#sensitivity, true positive rate
16/25
#specificity, true negative rate
54/74

install.packages("ROCR")
library(ROCR)

ROCR_pred <- prediction(predict_train, quality_train$PoorCare)
ROCR_perf <- performance(ROCR_pred, "tpr", "fpr")
plot(ROCR_perf)
plot(ROCR_perf, colorize = TRUE)
plot(ROCR_perf, colorize = TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7))

predict_test <- predict(quality_log, type="response", newdata=quality_test)
summary(predict_test)
table(quality_test$PoorCare, predict_test > 0.3)


ROCR_pred_test <- prediction(predict_test, quality_test$PoorCare)
auc <- as.numeric(performance(ROCR_pred_test, "auc")@y.values)
