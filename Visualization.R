library(rJava)
library(RMongo)
library(tm)
library(SnowballC)
library(topicmodels)
library(slam)
library(reshape2)
library(ggplot2)
library(LDAvis)

db <- mongoDbConnect('news')
print(dbShowCollections(db))
query <- dbGetQuery(db, "pravda_cleaned", "{}", skip=0, limit = 25000)
data <- query
rm(query)
data[nchar(data$text) > 0, 0]

doc.vec <- VectorSource(data$text)
doc.corpus <- VCorpus(doc.vec)

doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("russian"))
doc.corpus <- tm_map(doc.corpus, stripWhitespace)

TDM <- TermDocumentMatrix(doc.corpus)
DTM <- DocumentTermMatrix(doc.corpus, 
                          control = list(stemming = FALSE, stopwords = TRUE, minWordLength = 4,
                                         removeNumbers = TRUE, removePunctuation = TRUE))
rowTotals <- apply(DTM, 1, sum)
DTM.new   <- DTM[rowTotals> 0, ] 
lda <- LDA(DTM.new, 50)

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

item_presence <- unlist(lapply(doc.corpus, function(item) { item$meta$id %in% DTM.new[['dimnames']][['Docs']]}))
json_lda = topicmodels_json_ldavis(lda, doc.corpus[item_presence], DTM.new)
serVis(json_lda , out.dir = 'vis', open.browser = TRUE)
#Create a VCorpus object using the tm package's Corpus function.
#Convert this to a document term matrix using DocumentTermMatrix, also from tm.
#Run your model using topicmodel's LDA function.
#Convert the output into JSON format using topicmodels_json_ldavis. The function requires the output from steps 1-3.
#Visualise with LDAvis' serVis.