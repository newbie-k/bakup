library(tm)
library(proxy)
library(dplyr)

doc <- c('The sky is blue.',
         'The sun is bright today.',
         'The sun in the sky is bright.',
         'we can see the shining sun, the bright sun.')
doc_corpus <- Corpus(VectorSource(doc))
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
tdm <- TermDocumentMatrix(doc_corpus, control = control_list)
tdm

tf <- as.matrix(tdm)
tf
idf <- log(ncol(tf) / (rowSums(tf > 0)))  # log(num_document / num_occuring_in_docs_of_each_word)
idf

temp <- diag(idf)
tf_idf <- crossprod(tf, temp)
colnames(tf_idf) <- rownames(tf)
tf_idf

# Why normalize? not even move mu to 0?
tf_idf / sqrt(rowSums(tf_idf^2))

## Reading files
df <- read.table('https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt', header = FALSE)
df

df <- read.csv('Downloads/test.csv')
df

df <- read.csv2('Downloads/test.csv')
df

df <- read.table('Downloads/test.csv', header = FALSE, sep = ',')
df

text <- paste('Now, I understand that because I am concern on it.',
              'expectations for what we will achieve this year are low.',
              'But, Mister Speaker, I appreciate the constructive approach',
              'that you are other leaders took at the end of last year',
              'to pass a budget and make tax cuts permanent for working',
              'families. So I hope we can work together this year on some',
              'bipartisan priorities like criminal justice reform and',
              'helping people who are battling prescription drug abuse',
              'and heroin abuse. So, who knows, we might surprise the',
              'cynics again')
text

## Word tokenization
library(tidyverse)
library(tokenizers)

word <- tokenize_words(text)
word
length(word)

tab <- table(word)
tab <- tibble(word = names(tab), count = as.numeric(tab))
tab

## sentence
sentences <- tokenize_sentences(text)
sentences

sentence_words <- tokenize_words(sentences[[1]])
sentence_words

## test
length(sentence_words) ## number of sentences
length(sentence_words[[1]]) ## number of word in 1st sentence

sapply(sentence_words, length) ## number of word in every sentence

## Exploratory Analysis
base_url <- 'https://programminghistorian.org/assets/basic-text-processing-in-r'
url <- sprintf('%s/sotu_text/236.txt', base_url)
text <- paste(readLines(url), collapse = '\n')
text

word <- tokenize_words(text)
tab <- table(word[[1]])
tab <- data_frame(word = names(tab), count = as.numeric(tab))
tab <- arrange(tab, desc(count))
tab

wf <- read_csv(sprintf('%s/%s', base_url, 'word_frequency.csv'))
wf
