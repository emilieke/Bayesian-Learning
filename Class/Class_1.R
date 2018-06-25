# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Bayesian Learning")
# ----------------------------------------------------------------------------------------------------
# Install packages
install.packages('tm')
install.packages('wordcloud')
install.packages('e1071')

# Load file
rm(list=ls())
sms <- read.csv("sms.csv")
str(sms,stringsAsFactors=F)

str(sms)

library(tm)
corpus <- Corpus(VectorSource(sms$text))
print(corpus)

# Translate all letters to lower case
clean_corpus <- tm_map(corpus, tolower)
inspect(clean_corpus[1:3])

# Remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)

# Remove punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)

# Remove common non-content words
stopwords()[1:10]
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())

# Remove the excess white space
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

# Finally, convert documents into plain text
clean_corpus <- tm_map(clean_corpus, PlainTextDocument)

# Word cloud

# Obtain the indices of spam and ham messages
spam_indices <- which(sms$type == "spam")
spam_indices[1:3]
ham_indices <- which(sms$type == "ham")
ham_indices[1:3]

library(wordcloud)
wordcloud(clean_corpus[ham_indices], min.freq=40)
wordcloud(clean_corpus[spam_indices], min.freq=40)

# Build a spam filter

# Divide data set into training and test data
sms_train <- sms[1:4169,]
sms_test <- sms[4170:5559,]

# Divide the clean corpus
corpus_train <- clean_corpus[1:4169]
corpus_test <- clean_corpus[4170:5559]

# Create DocumentTermMatrix
sms_dtm <- DocumentTermMatrix(clean_corpus)
inspect(sms_dtm[1:4, 30:35])

# Divide the matrix into training and test rows
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

# Identify words appearing at least 5 times
five_times_words <- findFreqTerms(sms_dtm_train, 5)
length(five_times_words)
five_times_words[1:5]

# Create document-term matrices using frequent words
sms_dtm_train <- DocumentTermMatrix(corpus_train,control=list(dictionary = five_times_words))
sms_dtm_test <- DocumentTermMatrix(corpus_test,control=list(dictionary = five_times_words))

# Create function to count occurences
convert_count <- function(x)
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Convert document term matrices
sms_dtm_train <- apply(sms_dtm_train, 2, convert_count)
sms_dtm_train[1:4, 30:35]
sms_dtm_test <- apply(sms_dtm_test, 2, convert_count)
sms_dtm_test[1:4, 30:35]

# Create naive bayes classifier
library(e1071)
classifier <- naiveBayes(sms_dtm_train, sms_train$type)
class(classifier)

# Make prediction
predictions <- predict(classifier, newdata=sms_dtm_test)

# Evaluate prediction
table(predictions, sms_test$type)

