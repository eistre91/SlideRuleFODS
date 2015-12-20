install.packages("tm")
library(tm)

tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)

install.packages("SnowballC")
library(SnowballC)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
stopwords("english")[1:10]
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus <- tm_map(corpus, stemDocument)

frequencies <- DocumentTermMatrix(corpus)
findFreqTerms(frequencies, lowfreq=20)
sparse <- removeSparseTerms(frequencies, .995)

tweets_sparse <- as.data.frame(as.matrix(sparse))
colnames(tweets_sparse) <- make.names(colnames(tweets_sparse))

tweets_sparse$Negative <- tweets$Negative

library(caTools)
set.seed(123)

split <- sample.split(tweets_sparse$Negative, SplitRatio = 0.7)
train_sparse <- subset(tweets_sparse, split)
test_sparse <- subset(tweets_sparse, !split)

library(rpart)
library(rpart.plot)

tweet_CART <- rpart(Negative ~ ., data = train_sparse, method = "class")
prp(tweet_CART)
predict_CART <- predict(tweet_CART, newdata = test_sparse, type = "class")
table(test_sparse$Negative, predict_CART)
table(test_sparse$Negative)

library(randomForest)
set.seed(123)
tweet_RF <- randomForest(Negative ~ ., data = train_sparse)
predict_RF <- predict(tweet_RF, newdata = test_sparse)
table(test_sparse$Negative, predict_RF)

logreg <- glm(Negative ~ ., data=train_sparse, family = binomial)
predict_logreg <- predict(logreg, newdata = test_sparse, type = "response")
table(test_sparse$Negative, predict_logreg > 0.5)
