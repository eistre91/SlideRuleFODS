setwd("LogisticRegression/")

pd <- read.csv("PollingData.csv")
str(pd)
table(pd$Year)
summary(pd)

install.packages("mice")
library(mice)

simple <- pd[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)

imputed <- complete(mice(simple))
summary(imputed)

pd$Rasmussen <- imputed$Rasmussen
pd$SurveyUSA <- imputed$SurveyUSA

summary(pd)

pd_train <- subset(pd, Year == 2004 | Year == 2008)
pd_train
pd_test <- subset(pd, Year == 2012)
table(pd_train$Republican)

#take a smart baseline based on a single poll result
table(sign(pd_train$Rasmussen))
table(pd_train$Republican, sign(pd_train$Rasmussen))

cor(pd_train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

mod1 <- glm(Republican ~ PropR, data = pd_train, family=binomial)
summary(mod1)

pred1 <- predict(mod1, type="response")
table(pd_train$Republican, pred1 >= 0.5)

mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data = pd_train, family=binomial)
summary(mod2)
pred2 <- predict(mod2, type="response")
table(pd_train$Republican, pred2 >= 0.5)

table(pd_test$Republican, sign(pd_test$Rasmussen))

pred_test <- predict(mod2, newdata=pd_test, type="response")
table(pd_test$Republican, pred_test >= 0.5)

subset(pd_test, pred_test >= 0.5 & Republican == 0)
