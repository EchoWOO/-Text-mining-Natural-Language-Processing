#### Analyzing the emoji used when people talk about China and #maga. 
#### And sentimental analysis

library(rtweet) #https://cran.r-project.org/web/packages/rtweet/rtweet.pdf
library(tidyverse)

library(lubridate) #functions for date/time data
library(scales) #scales for data visualization
library(stringr) #string manipulation
library(tidytext) #for text mining
library(syuzhet) #corpus
library(emojifont)

library(twitteR) #for the rest API
#https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

library(streamR) #for the streaming API
library(ROAuth)
#https://cran.r-project.org/web/packages/streamR/streamR.pdf


emoji("thumbsup")

# Matching a single string against a single keyword
#   If regexpr finds a match, it will return the character position at which the match is found (in the example above, it will return 1).
#   If it does not find a match, regexpr will return -1.

keyword <- "<ed><a0><bd><ed><b1><8d>" # encoding of the "thumbs up emoji"
tweetText <- "<ed><a0><bd><ed><b1><8d>nothing is impossible https://t.co/AckwyTxVjR"

results <- regexpr(keyword,tweetText)

#take out the emoji code
str_replace()
#regex alphanumeric, any alpha numeric characters between < and >
reg <- "<[a-zA-Z0-9][a-zA-Z0-9]>"
tweetText <- streamedtweets
reggsub(reg," ",tweetText)#(what to look for, what to replace, input string)


# Matching multiple strings against a multiple keywords

emojiEncodings <- read.csv("D:/620/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/emoji-encodings.csv")
tweetTexts <- c("<ed><a0><bd><ed><b1><8d>nothing is impossible https://t.co/AckwyTxVjR","Yesterday was a huge day for our founders <e2><9c><8d><ef><b8><8f><ed><a0><be><ed><b7><a0><ed><a0><bd><ed><b2><a5> Thanks to the @MYOB and @BlueChilliGroup marketing and social media specialis<e2><80><a6>")

results <- sapply(emojiEncodings$streamApiEncodings, regexpr, tweetTexts) %>%
  data.frame()
colnames(results) <- emojiEncodings$emojiDescription

results["THUMBS UP SIGN"]
results["COLLISION SYMBOL"]




# STREAMING API 
#My key
access_token <- "903665668637249537-Kx9BGZA2Ese2BobU69SABu34iweVxpN"
access_secret <-"1J0Km5vFxtb0CyC0WpdYZNFTMkTEBLQrDxOf9GtpbbM84"
consumer_key <- "i6O7nxzsjbbKBD4184tmIW7Dg"
consumer_secret <- "hhcFRtLuycDhw9Mmnv9wxYQHVS6hV3f6mC4uFAGioIr4YogImj"

#Max's Key
access_token <- "2184943214-9rnbivSHpBBj3Ir00O6mOHccpc8A6Rzo8a6Pif5"
access_secret <-"ffTO5Ev3hhltHu7jldwB42RvYUzfOmsCNLCIgJMZrhB0m"
consumer_key <- "C2dLLGuSqjJb5S5VP12ug9oyh"
consumer_secret <- "H3SZyhWpbvIkGVAkVWgBHXcO3EuT9MrYo4Ky3h9ScIWaGs84sb"

oathfunction <- function(consumer_key, consumer_secret, access_token, access_secret){
  my_oauth <- ROAuth::OAuthFactory$new(consumerKey=consumer_key,
                                       consumerSecret=consumer_secret,
                                       oauthKey=access_token,
                                       oauthSecret=access_secret,
                                       needsVerifier=FALSE, handshakeComplete=TRUE,
                                       verifier="1",
                                       requestURL="https://api.twitter.com/oauth/request_token",
                                       authURL="https://api.twitter.com/oauth/authorize",
                                       accessURL="https://api.twitter.com/oauth/access_token",
                                       signMethod="HMAC")
  return(my_oauth)
}

my_oauth <- oathfunction(consumer_key, consumer_secret, access_token, access_secret)

# Set the parameters of your stream
# Keep in mind that most of these parameters will not work together
# For example, the location search cannot be paired with other parameters

file = "mytwitterstreamchina.json"       #The data will be saved to this file as long as the stream is running
track = c("China") # c("#maga")                 #"Search" by keyword(s)
follow = NULL #c("pepsi","cocacola","7up")
#"Search" by Twitter user(s)
#loc = c(39.867004, -74.955763, 40.137992, -75.280303) #c(-179, -70, 179, 70)             #Geographical bounding box -- (min longitute,min latitude,max longitute,max latitude)
loc = NULL
lang = NULL                             #Filter by language
timeout = NULL #1000                          #Maximum time (in miliseconds)
tweets = 30000 #1000                      #Maximum tweets (usually, it will be less)


filterStream(file.name = file, 
             track = track,
             follow = follow, 
             locations = loc, 
             language = lang,
             #timeout = timeout, 
             tweets = tweets, 
             oauth = my_oauth,
             verbose = TRUE)

streamedtweets <- parseTweets(file, verbose = FALSE)

# IMPORTANT - for the emojis to be properly encoded, you must use the line below
streamedtweets$text <- iconv(streamedtweets$text , from = "latin1", to = "ascii", sub = "byte")

# OPTIONAL - If you want to use the REST API (rtweet package) in your analysis, you should use this command to encode the emojis
restAPI$text <- iconv(restAPI$text , from = "latin1", to = "ascii", sub = "byte")

#filter the retweets
streamedtweets <- streamedtweets %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^RT'))

#Get the text of retweets 
streamedtweetstext <- streamedtweets$text
streamedtweet1<- dplyr::select(streamedtweets, source, text, created_at)

# Matching multiple strings against a multiple keywords

emojiEncodings <- read.csv("D:/620/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/emoji-encodings.csv")
#tweetTexts <- c("<ed><a0><bd><ed><b1><8d>nothing is impossible https://t.co/AckwyTxVjR","Yesterday was a huge day for our founders <e2><9c><8d><ef><b8><8f><ed><a0><be><ed><b7><a0><ed><a0><bd><ed><b2><a5> Thanks to the @MYOB and @BlueChilliGroup marketing and social media specialis<e2><80><a6>")

emojiresults <- sapply(emojiEncodings$streamApiEncodings, regexpr, streamedtweetstext) %>%
  data.frame()
colnames(emojiresults) <- emojiEncodings$emojiDescription

#emojiresults["THUMBS UP SIGN"]
#table(emojiresults["THUMBS UP SIGN"])
#emojiresults["ANGRY FACE"]
#table(emojiresults["ANGRY FACE"])
#table(emojiresults["ANGRY FACE"])

#Set no emoji to 0 but with emoji to 1
emojiresults[emojiresults == -1] <- 0
emojiresults[emojiresults != 0] <- 1

totalemoji <- colSums(emojiresults)

# Take out the top 20 emojis
topCNemoji <- sort(totalemoji, decreasing = TRUE)%>%
  head(20)

barplot(topCNemoji) +
  labs(x = "", y = "Frequency of Emojis", fill = "")

#ggplot(topCNemoji) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Frequency of Emojis", fill = "")

# Comparison 4: sentiment analysis

# but before analyzing the tweets, a primer on sentiment analysis using the syuzhet package
# https://cran.r-project.org/web/packages/syuzhet/syuzhet.pdf

# Basic sentiment analysis is quite simple - the words are compared against a corpus
# of "good" and "bad" words. Good words get a positive score. Bad words get a negative score.
# the average score across all words determines the overal sentiment.
wordByWordSentiment1 <- get_sentiment(c("nlp","is","awesome"), method="syuzhet")
overallSentiment1 <- mean(wordByWordSentiment1)

wordByWordSentiment2 <- get_sentiment(c("I","hate","gross","broccoli","it","sucks"), method="syuzhet")
overallSentiment2 <- mean(wordByWordSentiment2)


# The examples above use the "syuzhet" model, which only give a single positive/negative score
# Other sentiment models, such as "nrc", score sentiment across multiple dimensions

NRCSentiment <- get_nrc_sentiment(c("I","hate","gross","broccoli","it","sucks"))
overallNRCSentiment <- summarise_all(NRCSentiment,funs(mean))


# TOKENIZING
# Earlier, we tokenized the tweets into words using a regular expression
# The syuzhet package has built-in functions for tokenizing text

# We can tokenize a string into words
tokenizedWords <- get_tokens("What a happy joyous day!", pattern = "\\W")


# We can also tokenize a large text into sentences
###setwd("D:/620/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master")
sou <- readChar('state-of-the-union-2018.txt', file.info('state-of-the-union-2018.txt')$size,useByte = TRUE) %>%
  str_replace_all("[\r\n]" , " ") # for cleanup

tokenizedSentences <- get_sentences(sou)


# Likewise, we can run our sentiment analysis on words or on sentences
get_nrc_sentiment(tokenizedWords) %>%
  summarise_all(funs(mean))

get_nrc_sentiment(tokenizedSentences) %>%
  summarise_all(funs(mean))



# Sentiment analysis of @realdonaldtrump tweets

morningWords <- filter(djtWords, timeofday == 'Morning')
afternoonWords <- filter(djtWords, timeofday == 'Afternoon')
eveningWords <- filter(djtWords, timeofday == 'Evening')



# calculate the positive/negative sentiment of tweets from each group

scores <- data.frame()

score <- get_sentiment(eveningWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(timeofday="Evening",score=score))

score <- get_sentiment(afternoonWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(timeofday="Afternoon",score=score))

score <- get_sentiment(morningWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(timeofday="Morning",score=score))


ggplot(scores,aes(x=timeofday, y=score, fill = score > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Sentiment") +
  labs(title = "Sentiment of @realdonaldtrump tweets by time of day") +
  scale_fill_manual(name = "", labels = c("Negative", "Positive"),
                    values = c("red", "lightblue"))




# calculate the NRC sentiment scores

nrcScores <- data.frame()

eveningScores <- get_nrc_sentiment(eveningWords$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,data.frame(timeofday="Evening",eveningScores))

afternoonScores <- get_nrc_sentiment(afternoonWords$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,data.frame(timeofday="Afternoon",afternoonScores))

morningScores <- get_nrc_sentiment(morningWords$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,data.frame(timeofday="Morning",morningScores))



tallFormat = gather(nrcScores, key=emotion, value=score, anger:positive)

p <- ggplot(tallFormat,aes(x=timeofday, y=score), fill = "black") +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Sentiment") +
  labs(title = "NRC sentiment of @realdonaldtrump tweets by time of day")

scale_fill_manual(name = "", labels = c("", "",""),
                  values = c("red", "lightblue","orange"))

p + facet_wrap(~emotion, ncol = 5)




