library("readtext")
library("stringr")
library("quanteda")
library(dplyr)
library("stm")

setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/bild_articles/EU_Ref")

texts <- readtext("*.txt")
#tokenization & removing punctuation/numbers/URLs etc.
tokens <- texts$text %>%
  tokens(what = "word",
         remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"))

#applying relative pruning
dfm <- dfm_trim(dfm(tokens), min_docfreq = 0.005, max_docfreq = 0.99, 
                docfreq_type = "prop", verbose = TRUE)

dfm

topfeatures(dfm)

dfm_stm <- convert(dfm, to = "stm")

model <- stm(documents = dfm_stm$documents,
             vocab = dfm_stm$vocab, 
             K = 5,
             verbose = TRUE)

plot(model)

labelTopics(model,topics = c(1:5), n=5)

#Save top 20 features across topics and forms of weighting
labels <- labelTopics(model, n=20)
#only keep FREX weighting
topwords <- data.frame("features" = t(labels$frex))
#assign topic number as column name
colnames(topwords) <- paste("Topics", c(1:15))
#Return the result
topwords[1:5]