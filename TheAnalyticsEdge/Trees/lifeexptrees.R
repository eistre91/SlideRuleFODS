data(state)
statedata <- data.frame(state.x77)

linreg <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(linreg)
linreg_pred <- predict(linreg)
linreg_sse <- sum((linreg_pred - statedata$Life.Exp)^2)
sum(linreg$residuals^2)
linreg_sse

linreg2 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(linreg2)
sum(linreg2$residuals^2)

library(rpart)
library(rpart.plot)

tree <- rpart(Life.Exp ~ ., data = statedata)
prp(tree)
tree_pred <- predict(tree)
tree_sse <- sum((tree_pred - statedata$Life.Exp)^2)

tree <- rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(tree)
tree_pred <- predict(tree)
tree_sse <- sum((tree_pred - statedata$Life.Exp)^2)

tree <- rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(tree)
tree_pred <- predict(tree)
tree_sse <- sum((tree_pred - statedata$Life.Exp)^2)

library(caret)
set.seed(111)
tr.control <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp = seq(0.01, 0.50, 0.01))
tr <- train(Life.Exp ~ ., data = statedata, method="rpart", trControl = tr.control, tuneGrid = cp.grid)
tr

tree <- rpart(Life.Exp ~ ., data = statedata, cp = 0.12)
prp(tree)
tree_pred <- predict(tree)
tree_sse <- sum((tree_pred - statedata$Life.Exp)^2)

set.seed(111)
tr.control <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp = seq(0.01, 0.50, 0.01))
tr <- train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tree <- tr$finalModel
prp(tr$finalModel)
tree_pred <- predict(tree)
tree_sse <- sum((tree_pred - statedata$Life.Exp)^2)
