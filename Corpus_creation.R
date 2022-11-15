library(tm)
library(filehash)
library(stringi)
library(stringr)
library(dplyr)
library(quanteda)

setwd('C:/Users/Public/EU-Ref/')

## start with referendum files IRL 2008

# create a corpus out of a folder with txt-files
IRL_no<- SimpleCorpus(DirSource(directory="no", encoding="UTF-8"), control=list(language="english", tolower)) #creates corpus based on correct directory
save(IRL_no, file='Processed/IRL_2008_no_corpus.RDA')
IRL_no_corpus<- corpus(IRL_no) # creates a quanteda corpus in addition for counting tokens properly
stopwords<-stopwords("english") #sets a stopword list for English language
# convert corpus into a data frame
IRL_no_df<-data.frame(text = sapply(IRL_no, as.character), stringsAsFactors = FALSE)


## Extracting covariates from filenames is tricky here because naming wasn't very systematic when the main analysis was done
# create new columns for covariates
IRL_no_df$party <- NA
IRL_no_df$actor <- NA
IRL_no_df$camp <- NA
IRL_no_df$date <- NA

IRL_no_df <- cbind(rownames(IRL_no_df), IRL_no_df) # convert rownames into column
rownames(IRL_no_df) <- NULL
colnames(IRL_no_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
IRL_no_df$extract <- IRL_no_df$ID

# extract party names from filenames (IDs) (in extract column)
for (i in 1:239) {
  IRL_no_df$party[i] <- stri_extract_first_regex(tolower(IRL_no_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_no_df$extract <- gsub("^[[:alpha:]]+", "", tolower(IRL_no_df$extract))

# extract dates from remaining information in extract column
for (i in 1:239) {
  IRL_no_df$date[i] <- stri_extract_first_regex(tolower(IRL_no_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_no_df$extract <- gsub("^[[:digit:]]+", "", tolower(IRL_no_df$extract))

# extract actor names from remaining information in extract column)
IRL_no_df$extract <- gsub("_", "", tolower(IRL_no_df$extract))
for (i in 1:239) {
  IRL_no_df$actor[i] <- stri_extract_first_regex(tolower(IRL_no_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
IRL_no_df$extract<-NULL

# clean party column from web
IRL_no_df$party <- gsub("web", "", tolower(IRL_no_df$party))

# assign camp
IRL_no_df$camp<-"no"

save(IRL_no_df, file='Processed/IRL_no_2008.RDA')

# create a corpus out of a folder with txt-files
IRL_yes<- SimpleCorpus(DirSource(directory="yes", encoding="UTF-8"), control=list(language="english", tolower)) #creates corpus based on correct directory
save(IRL_yes, file='Processed/IRL_2008_yes_corpus.RDA')
IRL_yes_corpus<- corpus(IRL_yes) # creates a quanteda corpus in addition for counting tokens properly
stopwords<-stopwords("english") #sets a stopword list for English language
# convert corpus into a data frame
IRL_yes_df<-data.frame(text = sapply(IRL_yes, as.character), stringsAsFactors = FALSE)

# create new columns for covariates
IRL_yes_df$party <- NA
IRL_yes_df$actor <- NA
IRL_yes_df$camp <- NA
IRL_yes_df$date <- NA

IRL_yes_df <- cbind(rownames(IRL_yes_df), IRL_yes_df)
rownames(IRL_yes_df) <- NULL
colnames(IRL_yes_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
IRL_yes_df$extract <- IRL_yes_df$ID

# extract party names from filenames (IDs) (in extract column)
for (i in 1:269) {
  IRL_yes_df$party[i] <- stri_extract_first_regex(tolower(IRL_yes_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_yes_df$extract <- gsub("^[[:alpha:]]+", "", tolower(IRL_yes_df$extract))

# extract dates from remaining information in extract column
for (i in 1:269) {
  IRL_yes_df$date[i] <- stri_extract_first_regex(tolower(IRL_yes_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_yes_df$extract <- gsub("^[[:digit:]]+", "", tolower(IRL_yes_df$extract))

# extract actor names from remaining information in extract column)
IRL_yes_df$extract <- gsub("_", "", tolower(IRL_yes_df$extract))
for (i in 1:269) {
  IRL_yes_df$actor[i] <- stri_extract_first_regex(tolower(IRL_yes_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
IRL_yes_df$extract<-NULL

# assign camp
IRL_yes_df$camp<-"yes"

# clean party column from web
IRL_yes_df$party <- gsub("web", "", tolower(IRL_yes_df$party))

save(IRL_yes_df, file='Processed/IRL_yes_2008.RDA')

## Combined dataset
IRL_2008_df <- rbind(IRL_no_df, IRL_yes_df)

# extract year from date
IRL_2008_df$day <- NA
IRL_2008_df$month <- NA
IRL_2008_df$year <- NA

for (i in 1:508) {
  IRL_2008_df$year[i] <- stri_extract_last_regex(IRL_2008_df$date[i], "\\d{4}[^\\d]*$")
}

# remove unnecessary characters from extract column
IRL_2008_df$date <- gsub("....$", "", IRL_2008_df$date)

for (i in 1:508) {
  IRL_2008_df$month[i] <- stri_extract_last_regex(IRL_2008_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove unnecessary characters from extract column
IRL_2008_df$date <- gsub("..$", "", IRL_2008_df$date)

for (i in 1:508) {
  IRL_2008_df$day[i] <- stri_extract_last_regex(IRL_2008_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove date column
IRL_2008_df$date<-NULL

# save file
save(IRL_2008_df, file="Processed/IRL_2008.rda")
write.csv(IRL_2008_df, "Processed/IRL_2008.csv")

## referendum files IRL 2009

# create a corpus out of a folder with txt-files
IRL_2009_no<- SimpleCorpus(DirSource(directory="no2", encoding="UTF-8"), control=list(language="english", tolower)) #creates corpus based on correct directory
save(IRL_2009_no, file='Processed/IRL_2009_no_corpus.RDA')
IRL_2009_no_corpus<- corpus(IRL_2009_no) # creates a quanteda corpus in addition for counting tokens properly
stopwords<-stopwords("english") #sets a stopword list for German language
# convert corpus into a data frame
IRL_2009_no_df<-data.frame(text = sapply(IRL_2009_no, as.character), stringsAsFactors = FALSE)


# create new columns for covariates
IRL_2009_no_df$party <- NA
IRL_2009_no_df$actor <- NA
IRL_2009_no_df$camp <- NA
IRL_2009_no_df$date <- NA

IRL_2009_no_df <- cbind(rownames(IRL_2009_no_df), IRL_2009_no_df)
rownames(IRL_2009_no_df) <- NULL
colnames(IRL_2009_no_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
IRL_2009_no_df$extract <- IRL_2009_no_df$ID

# extract party names from filenames (IDs) (in extract column)
for (i in 1:nrow(IRL_2009_no_df)) {
  IRL_2009_no_df$party[i] <- stri_extract_first_regex(tolower(IRL_2009_no_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_2009_no_df$extract <- gsub("^[[:alpha:]]+", "", tolower(IRL_2009_no_df$extract))

# extract dates from remaining information in extract column
for (i in 1:nrow(IRL_2009_no_df)) {
  IRL_2009_no_df$date[i] <- stri_extract_first_regex(tolower(IRL_2009_no_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_2009_no_df$extract <- gsub("^[[:digit:]]+", "", tolower(IRL_2009_no_df$extract))

# extract actor names from remaining information in extract column)
IRL_2009_no_df$extract <- gsub("_", "", tolower(IRL_2009_no_df$extract))
for (i in 1:nrow(IRL_2009_no_df)) {
  IRL_2009_no_df$actor[i] <- stri_extract_first_regex(tolower(IRL_2009_no_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
IRL_2009_no_df$extract<-NULL

# clean party column from web
IRL_2009_no_df$party <- gsub("web", "", tolower(IRL_2009_no_df$party))

# assign camp
IRL_2009_no_df$camp<-"no"

save(IRL_2009_no_df, file='Processed/IRL_no_2009.RDA')

# create a corpus out of a folder with txt-files
IRL_2009_yes<- SimpleCorpus(DirSource(directory="yes2", encoding="UTF-8"), control=list(language="english", tolower)) #creates corpus based on correct working directory
save(IRL_2009_yes, file='Processed/IRL_2009_yes_corpus.RDA')
IRL_2009_yes_corpus<- corpus(IRL_2009_yes) # creates a quanteda corpus in addition for counting tokens properly
stopwords<-stopwords("english") #sets a stopword list for English language
# convert corpus into a data frame
IRL_2009_yes_df<-data.frame(text = sapply(IRL_2009_yes, as.character), stringsAsFactors = FALSE)

# create new columns for covariates
IRL_2009_yes_df$party <- NA
IRL_2009_yes_df$actor <- NA
IRL_2009_yes_df$camp <- NA
IRL_2009_yes_df$date <- NA

IRL_2009_yes_df <- cbind(rownames(IRL_2009_yes_df), IRL_2009_yes_df)
rownames(IRL_2009_yes_df) <- NULL
colnames(IRL_2009_yes_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
IRL_2009_yes_df$extract <- IRL_2009_yes_df$ID

# extract party names from filenames (IDs) (in extract column)
for (i in 1:nrow(IRL_2009_yes_df)) {
  IRL_2009_yes_df$party[i] <- stri_extract_first_regex(tolower(IRL_2009_yes_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_2009_yes_df$extract <- gsub("^[[:alpha:]]+", "", tolower(IRL_2009_yes_df$extract))

# extract dates from remaining information in extract column
for (i in 1:nrow(IRL_2009_yes_df)) {
  IRL_2009_yes_df$date[i] <- stri_extract_first_regex(tolower(IRL_2009_yes_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
IRL_2009_yes_df$extract <- gsub("^[[:digit:]]+", "", tolower(IRL_2009_yes_df$extract))

# extract actor names from remaining information in extract column)
IRL_2009_yes_df$extract <- gsub("_", "", tolower(IRL_2009_yes_df$extract))
for (i in 1:nrow(IRL_2009_yes_df)) {
  IRL_2009_yes_df$actor[i] <- stri_extract_first_regex(tolower(IRL_2009_yes_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
IRL_2009_yes_df$extract<-NULL

# assign camp
IRL_2009_yes_df$camp<-"yes"

# clean party column from web
IRL_2009_yes_df$party <- gsub("web", "", tolower(IRL_2009_yes_df$party))

save(IRL_2009_yes_df, file='Processed/IRL_yes_2009.RDA')

## Combined dataset
IRL_2009_df <- rbind(IRL_2009_no_df, IRL_2009_yes_df)

# extract year from date
IRL_2009_df$day <- NA
IRL_2009_df$month <- NA
IRL_2009_df$year <- NA

for (i in 1:nrow(IRL_2009_df)) {
  IRL_2009_df$year[i] <- stri_extract_last_regex(IRL_2009_df$date[i], "\\d{4}[^\\d]*$")
}

# remove unnecessary characters from extract column
IRL_2009_df$date <- gsub("....$", "", IRL_2009_df$date)

for (i in 1:nrow(IRL_2009_df)) {
  IRL_2009_df$month[i] <- stri_extract_last_regex(IRL_2009_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove unnecessary characters from extract column
IRL_2009_df$date <- gsub("..$", "", IRL_2009_df$date)

for (i in 1:nrow(IRL_2009_df)) {
  IRL_2009_df$day[i] <- stri_extract_last_regex(IRL_2009_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove date column
IRL_2009_df$date<-NULL

# save file
save(IRL_2009_df, file="Processed/IRL_2009.rda")
write.csv(IRL_2009_df, "Processed/IRL_2009.csv")

## French referendum 2005

# create a corpus out of a folder with txt-files
F_non<- SimpleCorpus(DirSource(directory="non", encoding="ISO-8859-1"), control=list(language="french", tolower)) #creates corpus based on correct directory
save(F_non, file='Processed/F_2005_non_corpus.RDA')
F_non_corpus<- corpus(F_non)
stopwords<-stopwords("french") #sets a stopword list for French language
# convert corpus into a data frame
F_non_df<-data.frame(text = sapply(F_non, as.character), stringsAsFactors = FALSE)

# create new columns for covariates
F_non_df$party <- NA
F_non_df$actor <- NA
F_non_df$camp <- NA
F_non_df$date <- NA

F_non_df <- cbind(rownames(F_non_df), F_non_df)
rownames(F_non_df) <- NULL
colnames(F_non_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
F_non_df$extract <- F_non_df$ID

# extract party names from filenames (IDs) (in extract column)
for (i in 1:nrow(F_non_df)) {
  F_non_df$party[i] <- stri_extract_first_regex(tolower(F_non_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
F_non_df$extract <- gsub("^[[:alpha:]]+", "", tolower(F_non_df$extract))

# extract dates from remaining information in extract column
for (i in 1:nrow(F_non_df)) {
  F_non_df$date[i] <- stri_extract_first_regex(tolower(F_non_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
F_non_df$extract <- gsub("^[[:digit:]]+", "", tolower(F_non_df$extract))

# prepare extract column for actor extraction for dutch names with 'van'
F_non_df$extract <- gsub("de ", "de_", tolower(F_non_df$extract))
F_non_df$extract <- gsub("le ", "le_", tolower(F_non_df$extract))

# extract actor names from remaining information in extract column)
F_non_df$extract <- gsub("_", "", tolower(F_non_df$extract))

for (i in 1:nrow(F_non_df)) {
  F_non_df$actor[i] <- stri_extract_first_regex(tolower(F_non_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
F_non_df$extract<-NULL

# clean party column from web
F_non_df$party <- gsub("web", "", tolower(F_non_df$party))

# assign camp
F_non_df$camp<-"no"

save(F_non_df, file='Processed/F_non_2005.RDA')

# create a corpus out of a folder with txt-files
F_oui<- SimpleCorpus(DirSource(directory="oui", encoding="ISO-8859-1"), control=list(language="french", tolower)) #creates corpus based on correct directory
save(F_oui, file='Processed/F_2005_oui_corpus.RDA')
F_oui_corpus <- corpus(F_oui)
stopwords<-stopwords("french") #sets a stopword list for French language
# convert corpus into a data frame
F_oui_df<-data.frame(text = sapply(F_oui, as.character), stringsAsFactors = FALSE)

# create new columns for covariates
F_oui_df$party <- NA
F_oui_df$actor <- NA
F_oui_df$camp <- NA
F_oui_df$date <- NA

F_oui_df <- cbind(rownames(F_oui_df), F_oui_df)
rownames(F_oui_df) <- NULL
colnames(F_oui_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
F_oui_df$extract <- F_oui_df$ID

# extract party names from IDs (in extract column)
for (i in 1:nrow(F_oui_df)) {
  F_oui_df$party[i] <- stri_extract_first_regex(tolower(F_oui_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
F_oui_df$extract <- gsub("^[[:alpha:]]+", "", tolower(F_oui_df$extract))

# extract dates from remaining information in extract column
for (i in 1:nrow(F_oui_df)) {
  F_oui_df$date[i] <- stri_extract_first_regex(tolower(F_oui_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
F_oui_df$extract <- gsub("^[[:digit:]]+", "", tolower(F_oui_df$extract))

# prepare extract column for actor extraction for french names with 'de', 'le'
F_oui_df$extract <- gsub("de ", "de_", tolower(F_oui_df$extract))
F_oui_df$extract <- gsub("le ", "le_", tolower(F_oui_df$extract))

# extract actor names from remaining information in extract column)
F_oui_df$extract <- gsub("_", "", tolower(F_oui_df$extract))

for (i in 1:nrow(F_oui_df)) {
  F_oui_df$actor[i] <- stri_extract_first_regex(tolower(F_oui_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
F_oui_df$extract<-NULL

# assign camp
F_oui_df$camp<-"yes"

# clean party column from web
F_oui_df$party <- gsub("web", "", tolower(F_oui_df$party))

save(F_oui_df, file='Processed/F_oui_2005.RDA')

F_2005_df <- rbind(F_non_df, F_oui_df)

# extract year from date
F_2005_df$day <- NA
F_2005_df$month <- NA
F_2005_df$year <- NA

for (i in 1:nrow(F_oui_df)) {
  F_2005_df$year[i] <- stri_extract_last_regex(F_2005_df$date[i], "\\d{4}[^\\d]*$")
}

# remove unnecessary characters from extract column
F_2005_df$date <- gsub("....$", "", F_2005_df$date)

for (i in 1:nrow(F_oui_df)) {
  F_2005_df$month[i] <- stri_extract_last_regex(F_2005_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove unnecessary characters from extract column
F_2005_df$date <- gsub("..$", "", F_2005_df$date)

for (i in 1:nrow(F_oui_df)) {
  F_2005_df$day[i] <- stri_extract_last_regex(F_2005_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove date column
F_2005_df$date<-NULL

# save file
save(F_2005_df, file="Processed/F_2005.rda")
write.csv(F_2005_df, "Processed/F_2005.csv")

## referendum NL 2005

# create a corpus out of a folder with txt-files
NL_nee<- SimpleCorpus(DirSource(directory="nee", encoding="ISO-8859-1"), control=list(language="dutch", tolower)) #creates corpus based on correct directory
save(NL_nee, file='Processed/NL_2005_nee_corpus.RDA')
NL_nee_corpus<- corpus(NL_nee)
stopwords<-stopwords("dutch") #sets a stopword list for German language
# convert corpus into a data frame
NL_nee_df<-data.frame(text = sapply(NL_nee, as.character), stringsAsFactors = FALSE)

# create new columns for covariates
NL_nee_df$party <- NA
NL_nee_df$actor <- NA
NL_nee_df$camp <- NA
NL_nee_df$date <- NA

NL_nee_df <- cbind(rownames(NL_nee_df), NL_nee_df)
rownames(NL_nee_df) <- NULL
colnames(NL_nee_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
NL_nee_df$extract <- NL_nee_df$ID

# extract party names from filenames (IDs) (in extract column)
for (i in 1:nrow( NL_nee_df)) {
  NL_nee_df$party[i] <- stri_extract_first_regex(tolower(NL_nee_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

NL_nee_df$party <- gsub("gwsamenzwak", "gw", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("lpfsamenzwak", "lpf", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("cusamenzwak", "cu", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("cuhs", "cu", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("cuwi", "cu", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("grondneegrondrust", "grondnee", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("grondneenrc", "grondnee", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("gwvk", "gw", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("pvddgrondrust", "pvdd", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("pvddparool", "pvdd", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("spad", "sp", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("spfd", "sp", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("spfind", "sp", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("spnd", "sp", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("spnrc", "sp", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("sptrouw", "sp", tolower(NL_nee_df$party))
NL_nee_df$party <- gsub("spvk", "sp", tolower(NL_nee_df$party))

# remove unnecessary characters from extract column
NL_nee_df$extract <- gsub("^[[:alpha:]]+", "", tolower(NL_nee_df$extract))

# extract dates from remaining information in extract column
for (i in 1:nrow( NL_nee_df)) {
  NL_nee_df$date[i] <- stri_extract_first_regex(tolower(NL_nee_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
NL_nee_df$extract <- gsub("^[[:digit:]]+", "", tolower(NL_nee_df$extract))

# prepare extract column for actor extraction for dutch names with 'van'
NL_nee_df$extract <- gsub("van ", "van_", tolower(NL_nee_df$extract))

# extract actor names from remaining information in extract column)
NL_nee_df$extract <- gsub("_", "", tolower(NL_nee_df$extract))

for (i in 1:nrow( NL_nee_df)) {
  NL_nee_df$actor[i] <- stri_extract_first_regex(tolower(NL_nee_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
NL_nee_df$extract<-NULL

# clean party column from web
NL_nee_df$party <- gsub("web", "", tolower(NL_nee_df$party))

# assign camp
NL_nee_df$camp<-"no"

save(NL_nee_df, file='Processed/NL_nee_2005.RDA')

# create a corpus out of a folder with txt-files
NL_ja<- SimpleCorpus(DirSource(directory="ja", encoding="ISO-8859-1"), control=list(language="french", tolower)) #creates corpus based on correct directory
save(NL_ja, file='Processed/NL_2005_ja_corpus.RDA')
NL_ja_corpus <- corpus(NL_ja)
stopwords<-stopwords("dutch") #sets a stopword list for Dutch language
# convert corpus into a data frame
NL_ja_df<-data.frame(text = sapply(NL_ja, as.character), stringsAsFactors = FALSE)

# create new columns for covariates
NL_ja_df$party <- NA
NL_ja_df$actor <- NA
NL_ja_df$camp <- NA
NL_ja_df$date <- NA

NL_ja_df <- cbind(rownames(NL_ja_df), NL_ja_df)
rownames(NL_ja_df) <- NULL
colnames(NL_ja_df) <- c("ID","text","party", "actor", "camp", "date")
# copy IDs in order to keep them after extraction
NL_ja_df$extract <- NL_ja_df$ID

# extract party names from filenames (IDs) (in extract column)
for (i in 1:nrow(NL_ja_df)) {
  NL_ja_df$party[i] <- stri_extract_first_regex(tolower(NL_ja_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

NL_ja_df$party <- gsub("d", "d66", tolower(NL_ja_df$party))
NL_ja_df$party <- gsub("cd66a", "cda", tolower(NL_ja_df$party))
NL_ja_df$party <- gsub("pvd66", "pvd", tolower(NL_ja_df$party))
NL_ja_df$party <- gsub("vvd66", "vvd", tolower(NL_ja_df$party))
NL_ja_df$extract <- gsub("^[[:alpha:]]+", "", tolower(NL_ja_df$extract))
NL_ja_df$extract <- gsub("66web", "", tolower(NL_ja_df$extract))

# remove unnecessary characters from extract column
NL_ja_df$extract <- gsub("^[[:alpha:]]+", "", tolower(NL_ja_df$extract))

# extract dates from remaining information in extract column
for (i in 1:nrow(NL_ja_df)) {
  NL_ja_df$date[i] <- stri_extract_first_regex(tolower(NL_ja_df$extract[i]), "^[[:digit:]]+", opts_regex = NULL)
}

# remove unnecessary characters from extract column
NL_ja_df$extract <- gsub("^[[:digit:]]+", "", tolower(NL_ja_df$extract))

# prepare extract column for actor extraction for dutch names with 'van'
NL_ja_df$extract <- gsub("van ", "van_", tolower(NL_ja_df$extract))

# extract actor names from remaining information in extract column)
NL_ja_df$extract <- gsub("_", "", tolower(NL_ja_df$extract))

for (i in 1:nrow(NL_ja_df)) {
  NL_ja_df$actor[i] <- stri_extract_first_regex(tolower(NL_ja_df$extract[i]), "^[[:alpha:]]+", opts_regex = NULL)
}

# remove extract column
NL_ja_df$extract<-NULL

# assign camp
NL_ja_df$camp<-"yes"

# clean party column from web
NL_ja_df$party <- gsub("web", "", tolower(NL_ja_df$party))

save(NL_ja_df, file='Processed/NL_ja_2005.RDA')

NL_2005_df <- rbind(NL_nee_df, NL_ja_df)

# extract year from date
NL_2005_df$day <- NA
NL_2005_df$month <- NA
NL_2005_df$year <- NA

for (i in 1:nrow(NL_2005_df)) {
  NL_2005_df$year[i] <- stri_extract_last_regex(NL_2005_df$date[i], "\\d{4}[^\\d]*$")
}

# remove unnecessary characters from extract column
NL_2005_df$date <- gsub("....$", "", NL_2005_df$date)

for (i in 1:nrow(NL_2005_df)) {
  NL_2005_df$month[i] <- stri_extract_last_regex(NL_2005_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove unnecessary characters from extract column
NL_2005_df$date <- gsub("..$", "", NL_2005_df$date)

for (i in 1:nrow(NL_2005_df)) {
  NL_2005_df$day[i] <- stri_extract_last_regex(NL_2005_df$date[i], "\\d{2}[^\\d]*$") 
}

# remove date column
NL_2005_df$date<-NULL

# save file
save(NL_2005_df, file="Processed/NL_2005.rda")
write.csv(NL_2005_df, "Processed/NL_2005.csv")