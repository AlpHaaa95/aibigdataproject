#Data Project Management

data <- read.csv("Dataset1_labeled_data.csv")
View(data)

#Dataset structure
str(data)

#The first step in processing text data involves creating a corpus,which refers to a collection of text documents (SMS messages)

library(NLP)
library(tm)
twitter_corpus <- Corpus(VectorSource(data$tweet))
print(twitter_corpus)

#Look at the contents of the corpus
inspect(twitter_corpus[1:3])

#Cleaning steps
twitter_clean<-tm_map(twitter_corpus, tolower)
twitter_clean<-tm_map(twitter_clean,removeNumbers)
twitter_clean<-tm_map(twitter_clean,removeWords,stopwords())
twitter_clean<-tm_map(twitter_clean,removePunctuation)
twitter_clean<-tm_map(twitter_clean,stripWhitespace)

#Messages in SMS corpus before the cleaning process
inspect(twitter_corpus[1:3])

#Messages in SMS corpus after the cleaning process
inspect(twitter_clean[1:3])

#Tokenization: splitting the messages into individual components
#Creating a sparse matrix (a tokenized corpus): rows of the matrix 
#indicate documents (that is, SMS messages) and the columns indicate terms (that is, words)

twitter_dtm <- DocumentTermMatrix(twitter_clean)

#Creating training and test datasets: 75% for training and 25% for testing
twitter_raw_train <- data[1:6196, ]
twitter_raw_test <- data[6197:24783, ]

#The document-term matrix:
twitter_dtm_train <- twitter_dtm[1:6196, ]
twitter_dtm_test <- twitter_dtm[6197:24783, ]

#The corpus
twitter_corpus_train <- twitter_clean[1:6196]
twitter_corpus_test <- twitter_clean[6197:24783]

#Subsets are representative of the complete set of SMS data
prop.table(table(data$class))
prop.table(table(twitter_raw_train$class))
prop.table(table(twitter_raw_test$class))

#Visualizing text data: word clouds
library(wordcloud)
wordcloud(twitter_corpus_train,min.freq = 30,random.order=FALSE, colors=brewer.pal(4,"Dark2"))

#Visualizing text data: word cloud of spams
#hate <- subset(twitter_raw_train , class == "hate_speech")
#hate_corpus <- Corpus(VectorSource(hate$tweet))
#hate_corpus_clean<-tm_map(hate_corpus, tolower)
#hate_corpus_clean<-tm_map(hate_corpus_clean,removeNumbers)
#hate_corpus_clean<-tm_map(hate_corpus_clean,removeWords,stopwords())
#hate_corpus_clean<-tm_map(hate_corpus_clean,removePunctuation)
#hate_corpus_clean<-tm_map(hate_corpus_clean,stripWhitespace)
#hate_dtm <- DocumentTermMatrix(hate_corpus_clean)


#Creating indicator features for frequent words
#We eliminate words that appear in less than 05 SMS messages;we save frequent terms for use later;
twitter_dict<-findFreqTerms(twitter_dtm_train ,5)

#we limit our training and test matrices to only the words in thepreceding dictionary
twitter_train<-DocumentTermMatrix(twitter_corpus_train,list(dictionary=twitter_dict))
twitter_test <- DocumentTermMatrix(twitter_corpus_test,list(dictionary = twitter_dict))

#The model will rely on categorical variables;
#Cells in the sparse matrix indicate a count of the times a word appears in a message;
#We should change this to a factor variable that simply indicates âyesâ or ânoâ depending on whether the word appears at all;
#The following code defines a function to convert counts to factors
convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1,2),labels=c("Hate Speech","Offensive language","neither"))
  return(x)
}


#We apply convert_counts to each of the columns in our sparse matrix
twitter_train <- apply(twitter_train, MARGIN = 2, convert_counts)
twitter_test <- apply(twitter_test, MARGIN = 2, convert_counts)

#Training the model: the Naive Bayes algorithm
#Presence or absence of words is used to estimate the probability that a given SMS message is spam
#Packages
#install.packages("e1071")
#install.packages("gmodels")
library(e1071)
library(gmodels)
twitter_classifier <- naiveBayes(twitter_train, twitter_raw_train$class)
twitter_pred <- predict(twitter_classifier, twitter_test)

View(twitter_test)

#Model Performance
CrossTable(twitter_pred,twitter_raw_test$class, prop.chisq=FALSE,chisq=FALSE,prop.t=FALSE,dnn = c("Predicted","Actual"))

