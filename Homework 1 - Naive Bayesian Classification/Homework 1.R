# ----------------------------------------------------------------------------------------------------
# HOMEWORK 1 
# Emilie Engen, 100356077
# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Bayesian Learning")
# ----------------------------------------------------------------------------------------------------
# Install packages
install.packages('tm')
install.packages('wordcloud')
install.packages('e1071')

# ----------------------------------------------------------------------------------------------------
# Choose a collection of text documents that are classified in several groups. For example, 
# you can construct a file with the subjects of a set of your emails classified as ???spam??? or ???ham???.

# Clear the workspace
rm(list=ls())

# Load train and test file
doc_train <- read.table('documents_train.txt', sep="\t", header=FALSE, fill=TRUE)
doc_test <- read.table('documents_test.txt', sep="\t", header=FALSE, fill=TRUE)

# Change headers
colnames(doc_train) = c('type','text');
colnames(doc_test) = c('type','text');

# Select two document types for the purpose of this analysis
doc_train<-doc_train[(doc_train$type=='sci.med' | doc_train$type=='sci.electronics'),]
doc_test<-doc_test[(doc_test$type=='sci.med' | doc_test$type=='sci.electronics'),]

# Combine the two data sets
doc_total <- rbind(doc_train,doc_test)

# Reduce the number of levels in the categorical variable
doc_total$type <- factor(doc_total$type)
str(doc_total)

# ----------------------------------------------------------------------------------------------------
# Construct a Corpus and clean it as shown in class with Case Study I.

# Include tm package
library(tm) 

# Construct Corpus
corpus <- Corpus(VectorSource(doc_total$text))
print(corpus)

# Transform all letters to lower case
clean_corpus <- tm_map(corpus, tolower)
inspect(clean_corpus[1:3])

# Remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
inspect(clean_corpus[1:3])

# Remove punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)
inspect(clean_corpus[1:3])

# Remove stop words
stopwords()[1:10]
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
inspect(clean_corpus[1:3])

# Remove white space
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
inspect(clean_corpus[1:3])

# Convert documents to plain text
clean_corpus <- tm_map(clean_corpus, PlainTextDocument)
inspect(clean_corpus[1:3])

# ----------------------------------------------------------------------------------------------------
# Obtain a word cloud for each group of texts.

# Obtain indices for spam and ham messages
med_indices <- which(doc_total$type == 'sci.med')
med_indices[1:3]

el_indices <- which(doc_total$type == 'sci.electronics')
el_indices[1:3]

# Create word cloud
library(wordcloud)
wordcloud(clean_corpus[med_indices], min.freq=100)
wordcloud(clean_corpus[el_indices], min.freq=100)

# ----------------------------------------------------------------------------------------------------
# Divide corpus into training and test data.
# ----------------------------------------------------------------------------------------------------
# The original data set

# Compute number of each document type
n_med <- sum(doc_total$type == 'sci.med');n_med
n_el <- sum(doc_total$type == 'sci.electronics');n_el
n_doc <- nrow(doc_total);n_doc

# Proportions of each type
n_med / n_doc
n_el / n_doc

# Fix the seed to have the same results
set.seed(1)

# Sample sizes of both data sets
n_train <- floor(0.75 * n_doc);n_train
n_test <- n_doc - n_train;n_test

# Obtain the indices of the observations in both data sets
indices_train <- sort(sample(1:n_doc,n_train))
length(indices_train)
head(indices_train)

indices_test <- setdiff(1:n_doc,indices_train)
length(indices_test)
head(indices_test)

# Obtain the train data set
doc_train <- doc_total[indices_train,]
head(doc_train)

# Obtain the test data set
doc_test <- doc_total[indices_test,]
head(doc_test)

# The proportions of each doc type
sum(doc_train$type == 'sci.med')/n_train
sum(doc_train$type == 'sci.electronics')/n_train

sum(doc_test$type == 'sci.med')/n_test
sum(doc_test$type == 'sci.electronics')/n_test

# Length of train and testing set
n_doc_train <- nrow(doc_train);n_doc_train
n_doc_test <- nrow(doc_test);n_doc_test

# The corpus data 

# Obtain the train data set
corpus_train <- clean_corpus[indices_train]
head(corpus_train)

# Obtain the test data set
corpus_test <- clean_corpus[indices_test]
head(corpus_test)

# Length of train and testing set
n_corpus_train <- length(corpus_train);n_corpus_train
n_corpus_test <- length(corpus_test);n_corpus_test

# ----------------------------------------------------------------------------------------------------
# Use the training data set to construct a document-term matrix using the most frequent words.

# Create a sparse matrix where rows refer to documents and columns to words
doc_dtm <- DocumentTermMatrix(clean_corpus)
inspect(doc_dtm[1:4, 30:35])

doc_train_dtm <- doc_dtm[indices_train,]
doc_test_dtm <- doc_dtm[indices_test,]

# Identify most frequent words
five_times_words <- findFreqTerms(doc_train_dtm, 5)
length(five_times_words)
five_times_words[1:5]

# Create document-term matrices using frequent words
doc_train_dtm <- DocumentTermMatrix(corpus_train, control=list(dictionary = five_times_words))
inspect(doc_train_dtm[1:4, 30:35])
doc_test_dtm <- DocumentTermMatrix(corpus_test, control=list(dictionary = five_times_words))
inspect(doc_test_dtm[1:4, 30:35])

# Function to convert count to factor variables
convert_count <- function(x)
{
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert count function to train and test set
doc_train_dtm <- apply(doc_train_dtm, 2, convert_count)
doc_train_dtm[1:4, 30:35]
doc_test_dtm <- apply(doc_test_dtm, 2, convert_count)
doc_test_dtm[1:4, 30:35]


# ----------------------------------------------------------------------------------------------------
# Construct a naive Bayes classifier using the document-term matrix and the known group for each text 
# in the training data. Examine the performance of the classifier to predict the most probable group 
# a posteriori for each text in the test data.

# Create Naive Bayes classifier
library(e1071)
classifier <- naiveBayes(doc_train_dtm, doc_train$type)
class(classifier)

# Estimation of the prior probabilities
classifier$apriori/n_train

# Evaluate performance of test data
predictions <- predict(classifier, newdata=doc_test_dtm)

# Number of docs classified in each group
table(predictions)
summary(predictions)

# Table of correct and incorrect classifications
table(predictions, doc_test$type)

# The test error and accuracy
error <- 1 - mean(predictions==doc_test$type,na.rm=TRUE);error
accuracy <- mean(predictions==doc_test$type,na.rm=TRUE);accuracy


# Probabilities of the classifications made
prob.predictions <- predict(classifier,newdata=doc_test_dtm,type ="raw")
head(prob.predictions)

colors.error <- c("cyan","deepskyblue4")[1*(predictions==doc_test$type)+1]
plot(1:n_test,prob.predictions[,1],col=colors.error,pch=20,type="p",xlab="Test sample",
     ylab="Probabilities")

# ----------------------------------------------------------------------------------------------------
