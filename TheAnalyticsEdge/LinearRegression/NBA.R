NBA <- read.csv("NBA_train.csv")
str(NBA)

table(NBA$W, NBA$Playoffs)
#how to figure out number of wins needed for playoffs
#analytically?
#use a quantile?

library(dplyr)

NBA <- tbl_df(NBA)
NBA
no_playoff_wins <- NBA %>% 
  select(W, Playoffs) %>%
  filter(Playoffs == 0) %>%
  select(W)

quantile(x = no_playoff_wins$W, .95)
ecdf(no_playoff_wins$W)(42)

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
wins_reg <- lm(W~PTSdiff, data = NBA)
summary(wins_reg)

points_reg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB +TOV + STL + BLK, data = NBA)
summary(points_reg)

SSE <- sum(points_reg$residuals^2)
SSE
RMSE <- sqrt(SSE / nrow(NBA))
RMSE
mean(NBA$PTS)

points_reg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(points_reg2)

points_reg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(points_reg3)

points_reg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(points_reg4)

SSE <- sum(points_reg4$residuals^2)
RMSE <- sqrt(SSE / nrow(NBA))
RMSE

NBA_test <- read.csv("NBA_test.csv")
str(NBA_test)

points_predictions <- predict(points_reg4, newdata = NBA_test)
points_predictions

SSE <- sum((points_predictions - NBA_test$PTS)^2)
SST <- sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 <- 1 - SSE/SST
R2
RMSE <- sqrt(SSE/nrow(NBA_test))
RMSE
