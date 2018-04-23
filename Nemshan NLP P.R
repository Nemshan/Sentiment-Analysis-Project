#libraries needed for this project
#mac users run sometimes into a problem loading some libraries so the next 3 lines solve the problems
options("java.home"="/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/lib")
Sys.setenv(LD_LIBRARY_PATH='$JAVA_HOME/server')
dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/lib/server/libjvm.dylib')
library(rJava)
library(tidyr)
library(tidytext)
library(qdap)
library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(radarchart)

############################################################################################
# read  and clean the data
df1 <- readRDS("tweets.rds")


# remove non alpha numeric characters 
df1$text <- iconv(df1$text, from = "UTF-8", to = "ASCII", sub = "")

clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  custom_stop_words <- c("facebook", "mark","zuckerberg ",'like','follow','live','new','via','rt',"Mark Zuckerberg",
                         'twitter','us','page' ,'now', 'anything', 'ask', 'can', 'want',"cambridge analytica",'cambridge','facebooks', 'analytica')
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stemDocument, language = "english")
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  removeNonAscii <- content_transformer(function(x) gsub("[^\x20-\x7E]", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeNonAscii)
  return(cleaned_corpus)
}

my_corpus <- VCorpus(VectorSource(df1$text))
cleaned_my_corpus <- clean_corpus(my_corpus)
############################################################################################
# unigram wordcloud

# TDM (term document matrix)
TDM_tweets <- TermDocumentMatrix(cleaned_my_corpus)
TDM_tweets_m <- as.matrix(TDM_tweets)

# get term frequencies
term_frequency <- rowSums(TDM_tweets_m)
# sort frequencies 
term_frequency <- sort(term_frequency, dec = TRUE)
# get top 30 most common words
top30 <- term_frequency[1:30]
top30_ordered <- top30[order(top30, decreasing = FALSE)]
barplot(top30_ordered, col = 'blue', las = 2, horiz = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# unigram wordcloud 
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5, 
          max.words = 500, colors = brewer.pal(8, "Paired"))


############################################################################################
# bigram wordcloud 

tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

# bigram TDM
bigram_tdm <- TermDocumentMatrix(cleaned_my_corpus, 
                                 control = list(tokenize = tokenizer))
bigram_tdm_m <- as.matrix(bigram_tdm)
# get term frequencies
term_frequency <- rowSums(bigram_tdm_m)
# sort frequencies 
term_frequency <- sort(term_frequency, dec = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
word_freqs[1:30,]
# bigram wordcloud 
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5, max.words = 300,
          colors = brewer.pal(8, "Paired"))



############################################################################################
# trigram wordcloud 

tokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 3, max = 3))

# bigram TDM
trigram_tdm <- TermDocumentMatrix(cleaned_my_corpus, 
                                  control = list(tokenize = tokenizer))
trigram_tdm <- as.matrix(trigram_tdm)
# get term frequencies
term_frequency <- rowSums(trigram_tdm)
# sort frequencies 
term_frequency <- sort(term_frequency, dec = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
word_freqs[1:30,]
# bigram wordcloud 
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5, max.words = 300,
          colors = brewer.pal(8, "Paired"))


############################################################################################
# tfidf wordcloud 


# tfidf TDM
tfidf_tdm <- TermDocumentMatrix(cleaned_my_corpus, 
                                control=list(weighting = weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)
# term frequencies
term_frequency <- rowSums(tfidf_tdm_m)
# sort term_frequency in descending order
term_frequency <- sort(term_frequency, dec = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5,
          max.words = 1000, colors=brewer.pal(8, "Paired"))

############################################################################################
# sentiment analysis



# read  and clean the data
setwd('/Users/NNNN/Documents/UNH/spring semester/DATA902 Analytics Methods/Text Mining PK/NLP Final Project')
fb <- read.csv('fbtweets.csv')


# remove non alpha numeric characters 
fb$text <- iconv(fb$text, from = "UTF-8", to = "ASCII", sub = "")

clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  custom_stop_words <- c("facebook", "mark","zuckerberg ",'like','follow','live','new','via','rt',"Mark Zuckerberg",
                         'twitter','us','page' ,'now', 'anything', 'ask', 'can', 'want',"cambridge analytica",'cambridge','facebooks', 'analytica')
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stemDocument, language = "english")
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  removeNonAscii <- content_transformer(function(x) gsub("[^\x20-\x7E]", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeNonAscii)
  return(cleaned_corpus)
}

my_corpus <- VCorpus(VectorSource(fb$text))
cleaned_my_corpus <- clean_corpus(my_corpus)



# tidy tdm
TDM_tweet <- TermDocumentMatrix(cleaned_my_corpus)
tweets_tidy <- tidy(TDM_tweet)

# bing lexicon
bing_lex <- get_sentiments("bing")

#using sentiment analysis with bing lexicon
tweets_bing_lex <- inner_join(tweets_tidy, bing_lex, by = c("term" = "word"))
tweets_bing_lex$sentiment_n <- ifelse(tweets_bing_lex$sentiment == "negative", -1, 1)
tweets_bing_lex$sentiment_value <- tweets_bing_lex$sentiment_n * tweets_bing_lex$count
aggregate_bing_data <- aggregate(tweets_bing_lex$sentiment_value, 
                          list(index = tweets_bing_lex$document), sum)
aggregate_bing_data$index <- as.numeric(aggregate_bing_data$index)
colnames(aggregate_bing_data) <- c("index", "bing_score")

#sentiment barchart for corpus
barplot(aggregate_bing_data$bing_score, names.arg = aggregate_bing_data$index)

#check postive and negative 
aggregate_bing_data$pos_or_neg <- aggregate_bing_data$bing_score
aggregate_bing_data$pos_or_neg[aggregate_bing_data$bing_score > 0] <- 1
aggregate_bing_data$pos_or_neg[aggregate_bing_data$bing_score < 0] <- -1
aggregate_bing_data$pos_or_neg[aggregate_bing_data$bing_score == 0] <- 0
pos_neg_counts <- table(aggregate_bing_data$pos_or_neg)
barplot(pos_neg_counts, main="Distribution of Sentiment", 
        xlab="Positive, Neutral, or Negative")

############################################################################################
# comparison/contrast word clouds 


# the  polarity
polarity <- counts(polarity(fb$text))[, "polarity"]
fb$polarity <- polarity

# split by polarity into pos and neg
pos_tweets <- fb[fb$polarity > 0, "title"]
neg_tweets <- fb[fb$polarity < 0, "title"]
pos_tweets <- paste(pos_tweets, collapse = " ")
neg_tweets <- paste(neg_tweets, collapse = " ")
tweets_pos_and_neg <- c(pos_tweets, neg_tweets)

#make tdm
tweets_p_n_corpus <- VCorpus(VectorSource(fb$text))
tweets_p_n_corpus <- clean_corpus(tweets_p_n_corpus)
TDM_final <- TermDocumentMatrix(tweets_p_n_corpus)
TDM_final_m <- as.matrix(TDM_final)



# commonality cloud
commonality.cloud(TDM_final_m, colors = brewer.pal(8, "Dark2"), max.words = 100)

# comparison cloud
comparison.cloud(TDM_final_m, colors = brewer.pal(8, "Dark2"), max.words = 400)

############################################################################################
#emotion analysis with NRC lexicon


# tidy tdm
tidy_tweets <- tidy(TermDocumentMatrix(cleaned_my_corpus))

#NRC lexicon
nrc_lex <- get_sentiments("nrc")

#join corpus and NRC
fb_nrc <- inner_join(tidy_tweets, nrc_lex, by = c("term" = "word"))

aggdata_nrc <- aggregate(fb_nrc$count, list(index = fb_nrc$sentiment), sum)
chartJSRadar(aggdata_nrc)
