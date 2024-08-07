####text mining

install.packages("NLP")

install.packages("tm")
install.packages("SnowballC")
install.packages("glmnet")

library(tm)
library(glmnet)
library(SnowballC)

topic_docs <- Corpus(
  DirSource('20news-train/comp.graphics',
            encoding = 'UTF-8')
)

summary(topic_docs[1:5])

inspect(topic_docs[[1]])

topic_docs[[1]]$meta

topic_docs[[1]]$content



topic_2_docs <- Corpus(
  DirSource('20news-train/rec.motorcycles',
            encoding = 'UTF-8')
)

binomial_docs <- c(
  as.list(topic_docs),
  as.list(topic_2_docs)
)

labels_1 <- replicate(length(topic_docs), 'comp.graphics')
labels_2 <- replicate(length(topic_2_docs), 'rec.motorcycles')
binomial_labels <- c(labels_1, labels_2)

length(binomial_labels) == length(binomial_docs)


example_doc <- binomial_docs[["38487"]]
print(example_doc)

##tokenisation that keeps punctuation
tokens <- Boost_tokenizer(example_doc)
summary(tokens)
print(tokens)

#tokenisation that gets rid of punctuation
tokens_2 <- MC_tokenizer(example_doc)
summary(tokens_2)
print(tokens_2)

removePunctuation(example_doc)

removePunctuation(tokens)


###lowercasing

tolower(example_doc)

example_doc <- tolower(example_doc)

##remove stopwords
stops <- stopwords('en')
print(stops)

removeWords(example_doc, stops)

##lowercasing and stopwords
removeWords(tolower(example_doc), stops)

removeWords(tolower(tokens_2), stops)


##stemming
stemDocument(example_doc)



##apply to multiple docs

binomial_docs <- Corpus(VectorSource(binomial_docs))
binomial_docs

#example of function
cleaned_binomial_docs <- tm_map(binomial_docs, removeWords, stopwords('en'))


#lowercase
cleaned_binomial_docs <- tm_map(binomial_docs, tolower)
#punctuation
cleaned_binomial_docs <- tm_map(cleaned_binomial_docs, removePunctuation)
#stopwords
cleaned_binomial_docs <- tm_map(cleaned_binomial_docs, removeWords, stopwords('en'))
#stem
cleaned_binomial_docs <- tm_map(cleaned_binomial_docs, stemDocument)

binomial_docs
cleaned_binomial_docs



##vectorisation

binomial_dtm <- DocumentTermMatrix(cleaned_binomial_docs)
binomial_dtm

inspect(binomial_dtm[1:3,])

doc_lengths <- lapply(as.list(cleaned_binomial_docs), nchar)
doc_length <- unlist(doc_lengths)
quantile(doc_length)




#binary weighting (if a word is present or not)
binomial_dtm_binary <- DocumentTermMatrix(cleaned_binomial_docs, control = list(weighting = weightBin))
inspect(binomial_dtm_binary[1:3,])


#TF-IDF weighting
binomial_dtm_tfidf <- DocumentTermMatrix(cleaned_binomial_docs, control=list(weighting = weightTfIdf))
inspect(binomial_dtm_tfidf[1:3,])


##removing sparse terms
removeSparseTerms(binomial_dtm, 0.98)


#############
#############
#############

observed_vocabulary <- unlist(binomial_dtm$dimnames)

binomial_train_dtm <- DocumentTermMatrix(cleaned_binomial_docs, control = list(dictionary=observed_vocabulary))

binomial_train_labels <- (binomial_labels == 'comp.graphics') * 1

binomial_model <- glmnet(binomial_train_dtm, binomial_train_labels, family = 'binomial')

topic_1_test_docs <- Corpus(DirSource('20news-test/comp.graphics', encoding = 'UTF-8'))

topic_2_test_docs <- Corpus(DirSource('20news-test/rec.motorcycles', encoding = 'UTF-8'))

binomial_test_docs <- c(as.list(topic_1_test_docs), as.list(topic_2_test_docs))

binomial_test_docs <- Corpus(VectorSource(binomial_test_docs))
labels_test_1 <- replicate(length(topic_1_test_docs), 'comp.graphics')
labels_test_2 <- replicate(length(topic_2_test_docs), 'rec.motorcycles')
binomial_test_labels <- c(labels_test_1, labels_test_2)
cleaned_binomial_test_docs <- tm_map(binomial_test_docs, tolower)
cleaned_binomial_test_docs <- tm_map(cleaned_binomial_test_docs, removePunctuation)
cleaned_binomial_test_docs <- tm_map(cleaned_binomial_test_docs, removeWords, stopwords('en'))
cleaned_binomial_test_docs <- tm_map(cleaned_binomial_test_docs, stemDocument)

binomial_test_dtm <- DocumentTermMatrix(cleaned_binomial_test_docs, control = list(dictionary=observed_vocabulary))

binomial_test_dtm <- data.matrix(binomial_test_dtm)

#predict

binomial_probabilities <- predict(binomial_model, binomial_test_dtm, s=tail(binomial_model$lambda, 1), type='response')
binomial_predictions <- ifelse(binomial_probabilities>0.5, 1, 0)

binomial_test_labels <- (binomial_test_labels == 'comp.graphics') * 1

binomial_classification_error <- mean(binomial_predictions != binomial_test_labels)
print(paste('Accuracy', 1-binomial_classification_error))


############

cleaned_topic_docs <- tm_map(topic_docs, tolower)
cleaned_topic_docs <- tm_map(cleaned_topic_docs, removePunctuation)
cleaned_topic_docs <- tm_map(cleaned_topic_docs, removeWords, stopwords('en'))

topic_dtm <- DocumentTermMatrix(cleaned_topic_docs)
findFreqTerms(topic_dtm, lowfreq = 100)

findAssocs(topic_dtm, 'software', corlimit = 0.75)













