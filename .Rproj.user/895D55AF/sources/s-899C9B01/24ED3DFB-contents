#Data Project Management

data <- read.csv("Dataset1_labeled_data.csv")
View(data)

#Dataset structure
str(data)

#The first step in processing text data involves creating a corpus,which refers to a collection of text documents (SMS messages)

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
hate <- subset(twitter_raw_train , type == "hate_speech")
hate_corpus <- Corpus(VectorSource(hate$text))
hate_corpus_clean<-tm_map(spam_corpus, tolower)
hate_corpus_clean<-tm_map(spam_corpus_clean,removeNumbers)
hate_corpus_clean<-tm_map(spam_corpus_clean,removeWords,stopwords())
hate_corpus_clean<-tm_map(spam_corpus_clean,removePunctuation)
hate_corpus_clean<-tm_map(spam_corpus_clean,stripWhitespace)
hate_dtm <- DocumentTermMatrix(spam_corpus_clean)