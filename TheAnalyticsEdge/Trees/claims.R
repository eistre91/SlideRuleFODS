claims <- read.csv("ClaimsData.csv")

table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
spl <- sample.split(claims$bucket2009, SplitRatio = 0.6)
claims_train <- subset(claims, spl)
claims_test <- subset(claims, !spl)

mean(claims_train$age)
table(claims_train$diabetes)/nrow(claims_train)

table(claims_test$bucket2009, claims_test$bucket2008)
penalty_matrix <- matrix(c(0, 1, 2, 3, 4,
                           2, 0, 1, 2, 3,
                           4, 2, 0, 1, 2,
                           6, 4, 2, 0, 1,
                           8, 6, 4, 2, 0), byrow = TRUE,
                          nrow = 5)
sum(as.matrix(table(claims_test$bucket2009, claims_test$bucket2008)) * penalty_matrix) / nrow(claims_test)

table(claims_test$bucket2009)
sum(as.matrix(table(claims_test$bucket2009)) * penalty_matrix) / nrow(claims_test)

library(rpart)
library(rpart.plot)

claims_tree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + bucket2008 + reimbursement2008, data = claims_train, method = "class", cp = 0.00005)
prp(claims_tree)
predict_test <- predict(claims_tree, newdata = claims_test, type = "class")
table(claims_test$bucket2009, predict_test)

sum(as.matrix(table(claims_test$bucket2009, predict_test)) * penalty_matrix) / nrow(claims_test)

claims_tree2 <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + bucket2008 + reimbursement2008, data = claims_train, method = "class", cp = 0.00005,
                      parms = list(loss = penalty_matrix))
predict_test2 <- predict(claims_tree2, newdata = claims_test, type = "class")
table(claims_test$bucket2009, predict_test2)
sum(as.matrix(table(claims_test$bucket2009, predict_test2)) * penalty_matrix) / nrow(claims_test)
