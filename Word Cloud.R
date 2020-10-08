#WORD CLOUD TECHNIQUE

#Step 1 : Create a text file

#Step 2 : Install and load the required packages
library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

#Step 3 : Text mining
text <- readLines(file.choose())        #load the data
docs <- Corpus(VectorSource(text))      #load the data as Corpus
inspect(docs)                           #inspect the content of data

#text transformation & cleanning the text
docs<-tm_map(docs, tolower)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeWords, stopwords('english'))
docs<-tm_map(docs,stemDocument)
docs<-tm_map(docs,stripWhitespace)

#Step 4 : Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Step 5 : Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Plot word Frequencies
barplot(d[1:15,]$freq, las = 2, names.arg = d[1:15,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
