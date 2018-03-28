#### Analyzing the emoji used when people talk about China and #maga. 
#### And sentimental analysis
library(dplyr)

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

library(data.table)


emoji("thumbsup")

#define plot theme
plotTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    panel.background = element_rect(fill = "white", color = "grey80"),
    panel.grid.major.y = element_line("white"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text.x = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.y = element_text(size=12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    axis.ticks.y = element_line(color="white")
)
}

# Matching a single string against a single keyword
#   If regexpr finds a match, it will return the character position at which the match is found (in the example above, it will return 1).
#   If it does not find a match, regexpr will return -1.

keyword <- "<ed><a0><bd><ed><b1><8d>" # encoding of the "thumbs up emoji"
tweetText <- "<ed><a0><bd><ed><b1><8d>nothing is impossible https://t.co/AckwyTxVjR"

results <- regexpr(keyword,tweetText)

##possible solutions for taking out the 
#take out the emoji code
str_replace()
#regex alphanumeric, any alpha numeric characters between < and >
reg <- "<[a-zA-Z0-9][a-zA-Z0-9]>"
tweetText <- streamedtweets
reggsub(reg," ",tweetText)#(what to look for, what to replace, input string)


# Matching text with Emoji stream API encoding
emojiEncodings <- read.csv("D:/620/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/emoji-encodings.csv")
tweetTexts <- c("<ed><a0><bd><ed><b1><8d>nothing is impossible https://t.co/AckwyTxVjR","Yesterday was a huge day for our founders <e2><9c><8d><ef><b8><8f><ed><a0><be><ed><b7><a0><ed><a0><bd><ed><b2><a5> Thanks to the @MYOB and @BlueChilliGroup marketing and social media specialis<e2><80><a6>")

resultsofemoji <- sapply(emojiEncodings$streamApiEncodings, regexpr, streamedtweetstext) %>%
  data.frame()
colnames(resultsofemoji) <- emojiEncodings$emojiDescription

##Take out all the tweet text with Emoji US
resultsUS <- resultsofemoji["REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S"] %>%
  cbind(streamedtweetstext) %>%
  setnames(old=c("REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S","streamedtweetstext"), new=c("detectresult", "tweet")) %>%
  mutate(detectresult = as.numeric(detectresult),
         tweet = as.character(tweet)) %>%
  dplyr::select(detectresult,tweet) %>%
  as.data.frame() %>%
  dplyr::filter(detectresult!= -1) %>%
  dplyr::select(tweet)
  

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

# StreamAPIencoding for REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S
# <ed><a0><bc><ed><b7><ba><ed><a0><bc><ed><b7><b8>

#emojiresults["THUMBS UP SIGN"]
#table(emojiresults["THUMBS UP SIGN"])
#emojiresults["ANGRY FACE"]
#table(emojiresults["ANGRY FACE"])
#table(emojiresults["ANGRY FACE"])

#Set no emoji to 0 but with emoji to 1
emojiresults[emojiresults == -1] <- 0
emojiresults[emojiresults != 0] <- 1

China <- colSums(emojiresults)
maga <- colSums(emojiresults)

# combine the total emoji of China and #maga in one data frame
totalemoji <- cbind(China,maga) %>%
  data.frame()
# Important!!! transform the rownames into the first columm or arrange is going to lose the emojis
totalemojiname <- tibble::rownames_to_column(totalemoji, "emoji")
# Sort descending and take the top 20 of US and CN share in common
ChinaUS <- dplyr::arrange(totalemojiname,desc(China),desc(maga))%>%
  head(20)

###Pipe everything together: top 20 emojis
top20emoji <- cbind(China,maga) %>%
  data.frame() %>%
  tibble::rownames_to_column("emoji") %>%
  dplyr::arrange(desc(China),desc(maga))%>%
  head(20)

###Facet wrap top 20 emojis
tallFormat = gather(top20emoji, key= country, value= frequency,China:maga)

palette <- c('#70284a','#0d585f')

newlable <- c(
  `China` = "China",
  `maga` = "#maga"
)

p <- ggplot(tallFormat,aes(x= country, y= frequency, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("value") +
  labs(title = "Emoji frequency comparison", subtitle = 'Between search keywords of "China" and "#maga"')+
  scale_fill_manual(name = "emojis",labels = c("China","#maga"),values = palette)+plotTheme() 

ModelFacet <- p + facet_wrap(~emoji, ncol = 4)
ModelFacet

# Comparison 4: sentiment analysis

##Take out all the tweet text with Emoji US(#maga)
resultsUS <- resultsofemoji["REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S"] %>%
  cbind(streamedtweetstext) %>%
  setnames(old=c("REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S","streamedtweetstext"), new=c("detectresult", "tweet")) %>%
  mutate(detectresult = as.numeric(detectresult),
         text = as.character(tweet)) %>%
  dplyr::select(detectresult,text) %>%
  as.data.frame() %>%
  dplyr::filter(detectresult!= -1) %>%
  dplyr::select(text)

##Take out for all the tweet text with Emoji US(China)
resultsUS <- emojiresults["REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S"] %>%
  cbind(streamedtweetstext) %>%
  setnames(old=c("REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S","streamedtweetstext"), new=c("detectresult", "tweet")) %>%
  mutate(detectresult = as.numeric(detectresult),
         text = as.character(tweet)) %>%
  dplyr::select(detectresult,text) %>%
  as.data.frame() %>%
  dplyr::filter(detectresult!= -1) %>%
  dplyr::select(text)

##possible solutions for taking out the emojis 
#take out the emoji code
#str_replace()
#regex alphanumeric, any alpha numeric characters between < and >
reg <- "<[a-zA-Z0-9][a-zA-Z0-9]>"
resultsUS$text <- gsub(reg," ",resultsUS$text)

###word frequency

# to tokenize the tweets into words, we will use a regular expression
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
# [^ CHARACTERGROUP ] = Matches any single character that is not in CHARACTERGROUP
# A-Za-z = any letter
# \d     = any numeric digit
# #@     = the # or @ symbols
#^ means string starts with 
# [^A-Za-z\\d#@'] = Match any character that is not alphanumeric and is not # or @
# This is how it determines where the breaks are between words
# Stop words - common words that convey little meaning, should be removed
stop_words$word

resultsUSWords <- resultsUS %>%
  filter(!str_detect(text, '^RT|^"')) %>%                                      # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))                     # remove stop words

# word frequency #maga
resultsUSWords %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip() +
  labs(title = "Word frequency of tweets talking about maga with US emoji")+plotTheme()

# word frequency China
resultsUSWords %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip() +
  labs(title = "Word frequency of tweets talking about China with US emoji")+plotTheme()

# but before analyzing the tweets, a primer on sentiment analysis using the syuzhet package
# https://cran.r-project.org/web/packages/syuzhet/syuzhet.pdf
# Basic sentiment analysis is quite simple - the words are compared against a corpus
# of "good" and "bad" words. Good words get a positive score. Bad words get a negative score.
# the average score across all words determines the overal sentiment.
# calculate the positive/negative sentiment of tweets from each group
scores <- data.frame()

score <- get_sentiment(resultsUSWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(keyword="#maga",score=score))

score <- get_sentiment(resultsUSWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(keyword="China",score=score))

ggplot(scores,aes(x=keyword, y=score, fill = score > 0)) +
  geom_bar(stat = "identity") +
  ylab("Sentiment") +
  labs(title = "Sentiment analysis of tweets using US emoji", subtitle = "when people talk about China or #maga",caption = "using syuzhet package") +
  scale_fill_manual(name = "", labels = c("Negative", "Positive"),
                    values = c("red", "lightblue"))



# calculate the NRC sentiment scores
nrcScores <- data.frame()

magaScores <- get_nrc_sentiment(resultsUSWords$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,data.frame(keyword="#maga",magaScores))

Chinascores <- get_nrc_sentiment(resultsUSWords$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,data.frame(keyword="China",Chinascores))


tallFormat = gather(nrcScores, key=emotion, value=score, anger:positive)

p <- ggplot(tallFormat,aes(x=keyword, y=score), fill = "black") +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Sentiment") +
  xlab("Keywords") +
  labs(title = "NRC sentiment analysis of tweets using US emojis", subtitle = "when people talk about China or #maga", caption = "using syuzhet package")+
  scale_fill_manual(name = score,labels = c("China","#maga"), values = c('#70284a','#0d585f'))+plotTheme()

resultFacet <- p + facet_wrap(~emotion, ncol = 5)
resultFacet


