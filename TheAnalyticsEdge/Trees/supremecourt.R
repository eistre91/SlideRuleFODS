stevens <- read.csv("stevens.csv")
str(stevens)

library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio=0.7)
train <- subset(stevens, spl == TRUE)
test <- subset(stevens, spl == FALSE)

library(rpart)
library(rpart.plot)

stevens_tree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data = train, method = "class", minbucket = 25)
prp(stevens_tree, cex = .5)

predict_CART <- predict(stevens_tree, newdata = test, type = "class")
table(test$Reverse, predict_CART)

library(ROCR)
predict_ROC <- predict(stevens_tree, newdata = test)
predict_ROC
pred <- prediction(predict_ROC[,2], test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

stevens_tree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data = train, method = "class", minbucket = 5)
prp(stevens_tree2, cex = .5)

stevens_tree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data = train, method = "class", minbucket = 100)
prp(stevens_tree3, cex = .5)

install.packages("randomForest")
library(randomForest)

train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)
stevens_forest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                               data = train, nodesize = 25, ntree = 200)

set.seed(200)
predict_forest <- predict(stevens_forest, newdata = test)
table(test$Reverse, predict_forest)

install.packages("caret")
install.packages("e1071")

library(caret)
library(e1071)

num_folds <- trainControl(method = "cv", number = 10)
cp_grid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = train, method = "rpart", trControl = num_folds, tuneGrid = cp_grid)

stevens_tree_cv <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                         data = train, method = "class", cp = 0.18)
predict_CV <- predict(stevens_tree_cv, newdata = test, type="class")
table(test$Reverse, predict_CV)

prp(stevens_tree_cv, cex = .5)
