library(tm)
library(dplyr)
library(quanteda)
library(lsa)

setwd('C:/USers/Public/EU-Ref/')

# create a corpus out of a folder with txt-files - start with Ireland 2008
IRL_2008<- SimpleCorpus(DirSource(directory="corpus/corpus_IRL2008", encoding="UTF-8"), control=list(language="english", tolower)) #creates corpus based on correct directory
stopwords<-stopwords("english") #sets a stopword list for English language
# convert corpus into a data frame
IRL_2008_df<-data.frame(text = sapply(IRL_2008, as.character), stringsAsFactors = FALSE)

# create DTM
DTM_IRL_2008<-DocumentTermMatrix(IRL_2008, control = list(removePunctuation = TRUE, removeNumbers=TRUE,
                                                      stopwords = TRUE))

# find most frequent terms for reduction (otherwise computation takes too long)
x <- findMostFreqTerms(DTM_IRL_2008, 1000)

x1<-as.data.frame(x[[1]])
x1$names<-rownames(x1)
colnames(x1)[1]<-'count'
x2<-as.data.frame(x[[2]])
x2$names<-rownames(x2)
colnames(x2)[1]<-'count'
x3<-as.data.frame(x[[3]])
x3$names<-rownames(x3)
colnames(x3)[1]<-'count'

x_sum<-rbind(x1,x2,x3)
x_sum<-x_sum[,-1]

x_sum<-x_sum[duplicated(x_sum)]

DTM_matrix <- as.matrix(DTM_IRL_2008)

DTM_short<-DTM_matrix[,x_sum]
DTM_short<-t(DTM_short)

# calculate cosine distance
cos_IRL2008<-cosine(DTM_short)

## repeat for Ireland 2009
# create a corpus out of a folder with txt-files - Ireland 2009
IRL_2009<- SimpleCorpus(DirSource(directory="corpus/corpus_IRL2009", encoding="UTF-8"), control=list(language="english", tolower)) #creates corpus based on correct directory
stopwords<-stopwords("english") #sets a stopword list for English language
# convert corpus into a data frame
IRL_2009_df<-data.frame(text = sapply(IRL_2009, as.character), stringsAsFactors = FALSE)

# create DTM
DTM_IRL_2009<-DocumentTermMatrix(IRL_2009, control = list(removePunctuation = TRUE, removeNumbers=TRUE,
                                                          stopwords = TRUE))
# find most frequent terms for reduction (otherwise computation takes too long)
x <- findMostFreqTerms(DTM_IRL_2009, 1000)

x1<-as.data.frame(x[[1]])
x1$names<-rownames(x1)
colnames(x1)[1]<-'count'
x2<-as.data.frame(x[[2]])
x2$names<-rownames(x2)
colnames(x2)[1]<-'count'
x3<-as.data.frame(x[[3]])
x3$names<-rownames(x3)
colnames(x3)[1]<-'count'

x_sum<-rbind(x1,x2,x3)
x_sum<-x_sum[,-1]

x_sum<-x_sum[duplicated(x_sum)]

DTM_matrix <- as.matrix(DTM_IRL_2009)

DTM_short<-DTM_matrix[,x_sum]
DTM_short<-t(DTM_short)

# calculate cosine distance
cos_IRL2009<-cosine(DTM_short)

## repeat for France 2005
# create a corpus out of a folder with txt-files - France
F_2005<- SimpleCorpus(DirSource(directory="corpus/corpus_F", encoding="UTF-8"), control=list(language="french", tolower)) #creates corpus based on correct directory
stopwords<-stopwords("french") #sets a stopword list for French language
# convert corpus into a data frame
F_2005_df<-data.frame(text = sapply(F_2005, as.character), stringsAsFactors = FALSE)

# create DTM
DTM_F_2005<-DocumentTermMatrix(F_2005, control = list(removePunctuation = TRUE, removeNumbers=TRUE,
                                                          stopwords = TRUE))
# find most frequent terms for reduction (otherwise computation takes too long)
x <- findMostFreqTerms(DTM_F_2005, 1000)

x1<-as.data.frame(x[[1]])
x1$names<-rownames(x1)
colnames(x1)[1]<-'count'
x2<-as.data.frame(x[[2]])
x2$names<-rownames(x2)
colnames(x2)[1]<-'count'
x3<-as.data.frame(x[[3]])
x3$names<-rownames(x3)
colnames(x3)[1]<-'count'

x_sum<-rbind(x1,x2,x3)
x_sum<-x_sum[,-1]

x_sum<-x_sum[duplicated(x_sum)]

DTM_matrix <- as.matrix(DTM_F_2005)

DTM_short<-DTM_matrix[,x_sum]
DTM_short<-t(DTM_short)

# calculate cosine distance
cos_F2005<-cosine(DTM_short)

## repeat for France 2005
# create a corpus out of a folder with txt-files - Netherlands
NL_2005<- SimpleCorpus(DirSource(directory="corpus/corpus_NL", encoding="UTF-8"), control=list(language="dutch", tolower)) #creates corpus based on correct directory
stopwords<-stopwords("dutch") #sets a stopword list for Dutch language
# convert corpus into a data frame
NL_2005_df<-data.frame(text = sapply(NL_2005, as.character), stringsAsFactors = FALSE)

# create DTM
DTM_NL_2005<-DocumentTermMatrix(NL_2005, control = list(removePunctuation = TRUE, removeNumbers=TRUE,
                                                      stopwords = TRUE))

# find most frequent terms for reduction (otherwise computation takes too long)
x <- findMostFreqTerms(DTM_NL_2005, 1000)

x1<-as.data.frame(x[[1]])
x1$names<-rownames(x1)
colnames(x1)[1]<-'count'
x2<-as.data.frame(x[[2]])
x2$names<-rownames(x2)
colnames(x2)[1]<-'count'
x3<-as.data.frame(x[[3]])
x3$names<-rownames(x3)
colnames(x3)[1]<-'count'

x_sum<-rbind(x1,x2,x3)
x_sum<-x_sum[,-1]

x_sum<-x_sum[duplicated(x_sum)]

DTM_matrix <- as.matrix(DTM_NL_2005)

DTM_short<-DTM_matrix[,x_sum]
DTM_short<-t(DTM_short)

# calculate cosine distance
cos_NL2005<-cosine(DTM_short)

# combine in one table
cos_total<-cbind(cos_F2005[,1],cos_NL2005[,1],cos_IRL2008[,1], cos_IRL2009[,1])

colnames(cos_total)<- c('France 2005','NL 2005','Ireland 2008','Ireland 2009')

r<- c(cos_F2005[3,2],cos_NL2005[3,2],cos_IRL2008[3,2], cos_IRL2009[3,2])

cos_total <- rbind(cos_total, r)

rownames(cos_total)<- c('treaty_control','no-camp vs. treaty','yes-camp vs. treaty','between-camp')

save(cos_total,file='Processed/cosine_overview.RDA')
