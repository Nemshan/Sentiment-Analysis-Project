#NLP Final Project
#Nemshan Alharthi

options("java.home"="/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib")
Sys.setenv(LD_LIBRARY_PATH='$JAVA_HOME/server')
dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/lib/server/libjvm.dylib')
library(rJava)

library(twitteR)

##input your credentials here
consumer_key <-""
consumer_secret <- ""
access_token <- ""
access_secret <-""


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweetss = twitteR::searchTwitter("#facebook", n = 3000,lang ='en', since = '2018-04-17', retryOnRateLimit = 1e3)


#convert to dataframe 
df <- twListToDF(tweetss) #extract the data frame save it 
saveRDS(df, file='tweets.rds')
df1 <- readRDS("tweets.rds")
library(dplyr)
#clean up any duplicate tweets from the data frame using 
dplyr::distinct(df1)

