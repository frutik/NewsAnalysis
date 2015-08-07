#http://videos.xrce.xerox.com/index.php/videos/index/759 "Probabilistic topic models of text and users" by David Blei
#http://videolectures.net/slsfs05_hofmann_lsvm/ Latent Semantic Variable Models by Thomas Hofmann

#http://www.r-bloggers.com/r-and-mongodb/
#http://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/

library(rJava)
library(RMongo)
library(tm)
library(SnowballC)
library(topicmodels)
library(slam)
library(reshape2)
library(ggplot2)

db <- mongoDbConnect('news_stemmed')
print(dbShowCollections(db))
query <- dbGetQuery(db, "pravda_ua", "{}", skip=0, limit = 20000)
data <- query
rm(query)

doc.vec <- VectorSource(data$text)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)

doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("russian"))

#doc.corpus <- tm_map(doc.corpus, stemDocument, "russian")

doc.corpus <- tm_map(doc.corpus, stripWhitespace)

inspect(doc.corpus[2])

TDM <- TermDocumentMatrix(doc.corpus)
TDM

DTM <- DocumentTermMatrix(doc.corpus, 
                          control = list(stemming = FALSE, stopwords = TRUE, minWordLength = 4,
                          removeNumbers = TRUE, removePunctuation = TRUE))
inspect(DTM[1:10,1:10])

str(DTM)

#Now we can start asking questions like: what are the most frequently occurring terms? Each of these words occurred more that 2000 times.
#findFreqTerms(TDM, 2000)
findFreqTerms(TDM, 2500)

#What about associations between words? Let’s have a look at what other words had a high association with “love”.
#findAssocs(TDM, "love", 0.8)
findAssocs(TDM, "восток", 0.4)


TDM.common = removeSparseTerms(TDM, 1)
dim(TDM)
dim(TDM.common)

inspect(TDM.common[1:12,1:24])


TDM.dense <- as.matrix(TDM.common)
object.size(TDM.common)
object.size(TDM.dense)

TDM.dense = melt(TDM.dense, value.name = "count")
head(TDM.dense)


ggplot(TDM.dense, aes(x = Docs, y = Terms, fill = log10(count))) +
       geom_tile(colour = "white") +
       scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
       ylab("") +
       theme(panel.background = element_blank()) +
       theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

rowTotals <- apply(DTM , 1, sum)
DTM.new   <- DTM[rowTotals> 0, ] 
lda <- LDA(DTM.new, 30)
terms(lda, 5)
topics(lda, 5)

ctm <- CTM(DTM.new, 30)


