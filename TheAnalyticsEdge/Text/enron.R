emails <- read.csv("energy_bids.csv", stringsAsFactors = FALSE)

emails$email[1]

table(emails$responsive)

library(tm)

corpus <- Corpus(VectorSource(emails$email))

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm

dtm <- removeSparseTerms(dtm, .97)
dtm

labeledTerms <- as.data.frame(as.matrix(dtm))
labeledTerms$responsive <- emails$responsive

library(caTools)
set.seed(144)
split <- sample.split(labeledTerms$responsive, SplitRatio = 0.7)
train <- subset(labeledTerms, split)
test <- subset(labeledTerms, !split)

library(rpart)
library(rpart.plot)

email_CART <- rpart(responsive ~ ., data = train, method = "class")
prp(email_CART)

pred <- predict(email_CART, newdata = test)
pred[1:10,]
pred.prob <- pred[,2]
pred.prob
table(test$responsive, pred.prob >= 0.5)
table(test$responsive)

library(ROCR)
pred_ROCR <- prediction(pred.prob, test$responsive)
perf_ROCR <- performance(pred_ROCR, "tpr", "fpr")
plot(perf_ROCR, colorize=TRUE)
performance(pred_ROCR, "auc")@y.values
