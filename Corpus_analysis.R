#### Basic corpus analytical operations on EU referendum dataset
library(tm)
library(dplyr)
library(quanteda)

setwd('C:/Users/Public/EU-Ref/')

## load corpora
load('Processed/F_2005_non_corpus.RDA')
F_non_corpus<- corpus(F_non) # creates a quanteda corpus in addition for counting tokens properly
load('Processed/F_2005_oui_corpus.RDA')
F_oui_corpus<- corpus(F_oui) # creates a quanteda corpus in addition for counting tokens properly
load('Processed/NL_2005_nee_corpus.RDA')
NL_nee_corpus<- corpus(NL_nee) # creates a quanteda corpus in addition for counting tokens properly
load('Processed/NL_2005_ja_corpus.RDA')
NL_ja_corpus<- corpus(NL_ja)
load('Processed/IRL_2008_no_corpus.RDA')
IRL_no_corpus<- corpus(IRL_no)
load('Processed/IRL_2008_yes_corpus.RDA')
IRL_yes_corpus<- corpus(IRL_yes)
load('Processed/IRL_2009_no_corpus.RDA')
IRL_2009_no_corpus<- corpus(IRL_2009_no)
load('Processed/IRL_2009_yes_corpus.RDA')
IRL_2009_yes_corpus<- corpus(IRL_2009_yes)

## create TDMs for all cases
stopwords<-stopwords("english") #sets a stopword list for English language

TDM_IRL_no<-TermDocumentMatrix(IRL_no, control = list(removePunctuation = TRUE,
                                                      stopwords = TRUE))

TDM_IRL_yes<-TermDocumentMatrix(IRL_yes, control = list(removePunctuation = TRUE,
                                                        stopwords = TRUE))

TDM_IRL_2009_no<-TermDocumentMatrix(IRL_2009_no, control = list(removePunctuation = TRUE,
                                                      stopwords = TRUE))

TDM_IRL_2009_yes<-TermDocumentMatrix(IRL_2009_yes, control = list(removePunctuation = TRUE,
                                                        stopwords = TRUE))

stopwords<-stopwords("french") #sets a stopword list for French language

TDM_F_2005_non<-TermDocumentMatrix(F_non, control = list(removePunctuation = TRUE,
                                                                stopwords = TRUE))

TDM_F_2005_oui<-TermDocumentMatrix(F_oui, control = list(removePunctuation = TRUE,
                                                                  stopwords = TRUE))

stopwords<-stopwords("dutch") #sets a stopword list for Dutch language

TDM_NL_2005_nee<-TermDocumentMatrix(NL_nee, control = list(removePunctuation = TRUE,
                                                         stopwords = TRUE))

TDM_NL_2005_ja<-TermDocumentMatrix(NL_ja, control = list(removePunctuation = TRUE,
                                                         stopwords = TRUE))

## create statistics
stat <- data.frame('country','year','camp','documents','types','tokens','art_number','art_share')
colnames(stat)<-stat[1,]
stat<- stat[-1,]

stat_F_n <- stat
x<-sum(tm_term_score(TDM_F_2005_non, c('art', 'article','articles')))
y<-x / sum(ntoken(F_non_corpus))
stat_F_n[1,]<- c('France','2005','non',nDocs(TDM_F_2005_non),nTerms(TDM_F_2005_non),sum(ntoken(F_non_corpus)), x, y)

stat_F_o <- stat
x<-sum(tm_term_score(TDM_F_2005_oui, c('art', 'article','articles')))
y<-x / sum(ntoken(F_oui_corpus))
stat_F_o[1,]<- c('France','2005','oui',nDocs(TDM_F_2005_oui),nTerms(TDM_F_2005_oui),sum(ntoken(F_oui_corpus)), x, y)

stat_NL_n <- stat
x<-sum(tm_term_score(TDM_NL_2005_nee, c('art', 'artikel','artikelen')))
y<-x / sum(ntoken(NL_nee_corpus))
stat_NL_n[1,]<- c('Netherlands','2005','nee',nDocs(TDM_NL_2005_nee),nTerms(TDM_NL_2005_nee),sum(ntoken(NL_nee_corpus)), x, y)

stat_NL_j <- stat
x<-sum(tm_term_score(TDM_NL_2005_ja, c('art', 'artikel','artikelen')))
y<-x / sum(ntoken(NL_ja_corpus))
stat_NL_j[1,]<- c('Netherlands','2005','ja',nDocs(TDM_NL_2005_ja),nTerms(TDM_NL_2005_ja),sum(ntoken(NL_ja_corpus)), x, y)

stat_I_n <- stat
x<-sum(tm_term_score(TDM_IRL_no, c('art', 'article','articles')))
y<-x / sum(ntoken(IRL_no_corpus))
stat_I_n[1,]<- c('Ireland','2008','no',nDocs(TDM_IRL_no),nTerms(TDM_IRL_no),sum(ntoken(IRL_no_corpus)), x, y)

stat_I_y <- stat
x<-sum(tm_term_score(TDM_IRL_yes, c('art', 'article','articles')))
y<-x / sum(ntoken(IRL_yes_corpus))
stat_I_y[1,]<- c('Ireland','2008','yes',nDocs(TDM_IRL_yes),nTerms(TDM_IRL_yes),sum(ntoken(IRL_yes_corpus)), x, y)

stat_I_n2 <- stat
x<-sum(tm_term_score(TDM_IRL_2009_no, c('art', 'article','articles')))
y<-x / sum(ntoken(IRL_2009_no_corpus))
stat_I_n2[1,]<- c('Ireland','2009','no',nDocs(TDM_IRL_2009_no),nTerms(TDM_IRL_2009_no),sum(ntoken(IRL_2009_no_corpus)), x, y)

stat_I_y2 <- stat
x<-sum(tm_term_score(TDM_IRL_2009_yes, c('art', 'article','articles')))
y<-x / sum(ntoken(IRL_2009_yes_corpus))
stat_I_y2[1,]<- c('Ireland','2009','yes',nDocs(TDM_IRL_2009_yes),nTerms(TDM_IRL_2009_yes),sum(ntoken(IRL_2009_yes_corpus)), x, y)

stat <- rbind(stat_F_n,stat_F_o, stat_NL_n, stat_NL_j, stat_I_n, stat_I_y, stat_I_n2, stat_I_y2)

save(stat,file='Processed/corpus_statistics.RDA')
write.csv(stat,'Processed/corpus_statistics.CSV')
