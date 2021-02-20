library(readr)

#Download the dataset

data <- read_csv("offensive_hate_words.txt")
View(data)

#Exclude useless rows

offensive <- data[16:419, ]
View(offensive)
hate <- data[422:809, ]
View(hate)

library("dplyr")

# Clean the data frame "Hate"

hate <- gsub("type=homophobic	strength=strong", "", hate$`Abuse word lists`)
hate <- gsub("strength=medium","",hate)
hate <- gsub("type=racist	strength=strong","",hate)
hate <- gsub("strength=strong","",hate)
hate <- gsub("type=sexist","",hate)
hate <- gsub("type=racist","",hate)
hate <- gsub("strength=mild","",hate)

hate <- as.data.frame(hate)

View(hate)

# Create a hate corpus

hate_corpus <- Corpus(VectorSource(hate))

