#http://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/
#http://www.r-bloggers.com/r-and-mongodb/


library(rJava)
library(RMongo)
library(tm)
library(SnowballC)

db <- mongoDbConnect('news')
print(dbShowCollections(db))
query <- dbGetQuery(db, "pravda", "{}", 0, 5)
data <- query

doc.vec <- VectorSource(data$text)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)

doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("russian"))

library(SnowballC)
doc.corpus <- tm_map(doc.corpus, stemDocument)

doc.corpus <- tm_map(doc.corpus, stripWhitespace)

inspect(doc.corpus[2])

TDM <- TermDocumentMatrix(doc.corpus)
TDM

