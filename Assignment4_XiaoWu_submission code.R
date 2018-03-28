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
#emoji("thumbsup")

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

# Matching text with Emoji stream API encoding
emojiEncodings <- read.csv("D:/620/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/MUSA-620-Week-9-master/emoji-encodings.csv")

resultsofemoji <- sapply(emojiEncodings$streamApiEncodings, regexpr, streamedtweetstext) %>%
  data.frame()
colnames(resultsofemoji) <- emojiEncodings$emojiDescription

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

file = "mytwitterstreamchina1.json"       #The data will be saved to this file as long as the stream is running
track = c("China") # c("#maga")                 #"Search" by keyword(s)
follow = NULL #c("pepsi","cocacola","7up")
#"Search" by Twitter user(s)
#loc = c(39.867004, -74.955763, 40.137992, -75.280303) #c(-179, -70, 179, 70)             #Geographical bounding box -- (min longitute,min latitude,max longitute,max latitude)
loc = NULL
lang = NULL                             #Filter by language
timeout = NULL #1000                          #Maximum time (in miliseconds)
tweets = 50000 #1000                      #Maximum tweets (usually, it will be less)


filterStream(file.name = file, 
             track = track,
             follow = follow, 
             locations = loc, 
             language = lang,
             #timeout = timeout, 
             tweets = tweets, 
             oauth = my_oauth,
             verbose = TRUE)

###############For Key word "China"######################
streamedtweets <- parseTweets("mytwitterstreamchina.json", verbose = FALSE)
streamedtweets1 <- parseTweets("mytwitterstreamchina1.json", verbose = FALSE)

# IMPORTANT - for the emojis to be properly encoded, you must use the line below
streamedtweets$text <- iconv(streamedtweets$text , from = "latin1", to = "ascii", sub = "byte")
streamedtweets1$text <- iconv(streamedtweets1$text , from = "latin1", to = "ascii", sub = "byte")

streamedtweetschina <- rbind(streamedtweets,streamedtweets1)

# OPTIONAL - If you want to use the REST API (rtweet package) in your analysis, you should use this command to encode the emojis
restAPI$text <- iconv(restAPI$text , from = "latin1", to = "ascii", sub = "byte")

#filter the retweets
streamedtweetschina <- streamedtweetschina %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^RT'))

#Get the text of retweets 
streamedtweetschinatext <- streamedtweetschina$text
streamedtweetschina<- dplyr::select(streamedtweets, source, text, created_at)

# Matching multiple strings against a multiple keywords

emojiresultschina <- sapply(emojiEncodings$streamApiEncodings, regexpr, streamedtweetschinatext) %>%
  data.frame()
colnames(emojiresultschina) <- emojiEncodings$emojiDescription

# StreamAPIencoding for REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S
# <ed><a0><bc><ed><b7><ba><ed><a0><bc><ed><b7><b8>

#emojiresults["THUMBS UP SIGN"]
#table(emojiresults["THUMBS UP SIGN"])
#emojiresults["ANGRY FACE"]
#table(emojiresults["ANGRY FACE"])
#table(emojiresults["ANGRY FACE"])

#Set no emoji to 0 but with emoji to 1
emojiresultschina[emojiresultschina == -1] <- 0
emojiresultschina[emojiresultschina != 0] <- 1

China <- colSums(emojiresultschina)


###############For Key word "#maga"######################
streamedtweets2 <- parseTweets("mytwitterstreammaga.json", verbose = FALSE)
streamedtweets3 <- parseTweets("mytwitterstreammaga1.json", verbose = FALSE)

# IMPORTANT - for the emojis to be properly encoded, you must use the line below
streamedtweets2$text <- iconv(streamedtweets2$text , from = "latin1", to = "ascii", sub = "byte")
streamedtweets3$text <- iconv(streamedtweets3$text , from = "latin1", to = "ascii", sub = "byte")

streamedtweetsmaga <- rbind(streamedtweets2,streamedtweets3)

# OPTIONAL - If you want to use the REST API (rtweet package) in your analysis, you should use this command to encode the emojis
restAPI$text <- iconv(restAPI$text , from = "latin1", to = "ascii", sub = "byte")

#filter the retweets
streamedtweetsmaga <- streamedtweetsmaga %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^RT'))

#Get the text of retweets 
streamedtweetsmagatext <- streamedtweetsmaga$text
streamedtweetsmaga<- dplyr::select(streamedtweets, source, text, created_at)

# Matching multiple strings against a multiple keywords

emojiresultsmaga <- sapply(emojiEncodings$streamApiEncodings, regexpr, streamedtweetsmagatext) %>%
  data.frame()
colnames(emojiresultsmaga) <- emojiEncodings$emojiDescription

#Set no emoji to 0 but with emoji to 1
emojiresultsmaga[emojiresultsmaga == -1] <- 0
emojiresultsmaga[emojiresultsmaga != 0] <- 1

maga <- colSums(emojiresultsmaga)

####################################################
# combine the total emoji of China and #maga in one data frame
#totalemoji <- cbind(China,maga) %>%
#  data.frame()
# Important!!! transform the rownames into the first columm or arrange is going to lose the emojis
#totalemojiname <- tibble::rownames_to_column(totalemoji, "emoji")
# Sort descending and take the top 20 of US and CN share in common
#ChinaUS <- dplyr::arrange(totalemojiname,desc(China),desc(maga))%>%
#  head(20)

###Pipe everything together: top 20 emojis
top20emoji <- cbind(China,maga) %>%
  data.frame() %>%
  tibble::rownames_to_column("emoji") %>%
  dplyr::arrange(desc(maga),desc(China))%>%
  head(20)

###Facet wrap top 20 emojis
tallFormat = gather(top20emoji, key= keywords, value= frequency,China:maga)

palette <- c('#70284a','#0d585f')

newlable <- c(
  `China` = "China",
  `maga` = "#maga"
)

p <- ggplot(tallFormat,aes(x= keywords, y= frequency, fill = keywords)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("value") +
  labs(title = "Emoji frequency comparison", subtitle = 'Between search keywords of "China" and "#maga"')+
  scale_fill_manual(name = "emojis",labels = c("China","#maga"),values = palette)+plotTheme() 

ModelFacet <- p + facet_wrap(~emoji, ncol = 4)
ModelFacet

################# word frequency analysis############################
##Take out all the tweet text with Emoji US(#maga)
resultsUSmaga <- emojiresultsmaga["REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S"] %>%
  cbind(streamedtweetsmagatext) %>%
  setnames(old=c("REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S","streamedtweetsmagatext"), new=c("detectresult", "tweet")) %>%
  mutate(detectresult = as.numeric(detectresult),
         text = as.character(tweet)) %>%
  dplyr::select(detectresult,text) %>%
  as.data.frame() %>%
  dplyr::filter(detectresult!= 0) %>%
  dplyr::select(text)

##Take out for all the tweet text with Emoji US(China)
resultsUSchina <- emojiresultschina["REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S"] %>%
  cbind(streamedtweetschinatext) %>%
  setnames(old=c("REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S","streamedtweetschinatext"), new=c("detectresult", "tweet")) %>%
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
resultsUSmaga$text <- gsub(reg," ",resultsUSmaga$text)
resultsUSchina$text <- gsub(reg," ",resultsUSchina$text)

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

# word frequency #maga
resultsUSmagaWords <- resultsUSmaga %>% 
  filter(!str_detect(text, '^RT|^"')) %>%                                           # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]")) %>%                 # remove stop words
  filter(!word =="#maga")

resultsUSmagaWords %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity",aes(fill = n)) +
  scale_fill_gradient(low = "#7bbcb0", high = "#123f5a", name = "Occurrences \nof words") +
  ylab("Occurrences") +
  coord_flip() +
  labs(title = "Word frequency of tweets with US emoji",subtitle = " when people are talking about #maga" )+plotTheme()

# word frequency china
resultsUSchinaWords <- resultsUSchina %>% 
  filter(!str_detect(text, '^RT|^"')) %>%                                           # filter tweets starting w/ "rt"
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%   # remove links
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%                     # parse the text into "tokens"
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]")) %>%                 # remove stop words
  filter(!word =="china")

resultsUSchinaWords %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity",aes(fill = n)) +
  scale_fill_gradient(low = "#ffc6c4", high = "#672044", name = "Occurrences \nof words") +
  ylab("Occurrences") +
  coord_flip() +
  labs(title = "Word frequency of tweets with US emoji",subtitle = " when people are talking about China" )+plotTheme()


# but before analyzing the tweets, a primer on sentiment analysis using the syuzhet package
# https://cran.r-project.org/web/packages/syuzhet/syuzhet.pdf
# Basic sentiment analysis is quite simple - the words are compared against a corpus
# of "good" and "bad" words. Good words get a positive score. Bad words get a negative score.
# the average score across all words determines the overal sentiment.
# calculate the positive/negative sentiment of tweets from each group


# 1:00 a.m. to 5a.m. Mar.28th, 2018
scores <- data.frame()

score <- get_sentiment(resultsUSmagaWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(keyword="#maga",score=score))

score <- get_sentiment(resultsUSchinaWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(keyword="China",score=score))

# average sentiment scores
ggplot(scores,aes(x=keyword, y=score, fill = score > 0)) +
  geom_bar(stat = "identity") +
  ylab("Sentiment") +
  labs(title = "Sentiment analysis of tweets using the 'US' emoji", subtitle = "when people talk about China or #maga \nbetween 1:00 a.m. to 5a.m. Mar.28th, 2018",caption = "using syuzhet package") +
  scale_fill_manual(name = "", labels = c("Positive","Negative"),
                    values = c("lightblue","red"))


# 2:00 a.m. to 6a.m. Mar.27th, 2018
scores <- data.frame()

score <- get_sentiment(resultsUSmagaWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(keyword="#maga",score=score))

score <- get_sentiment(resultsUSchinaWords$word, method="syuzhet") %>%
  mean()
scores <- rbind(scores,data.frame(keyword="China",score=score))

# average sentiment scores
ggplot(scores,aes(x=keyword, y=score, fill = score > 0)) +
  geom_bar(stat = "identity") +
  ylab("Sentiment") +
  labs(title = "Sentiment analysis of tweets using the 'US' emoji", subtitle = "when people talk about China or #maga \nbetween 2:00 a.m. to 6a.m. Mar.27th, 2018",caption = "using syuzhet package") +
  scale_fill_manual(name = "", labels = c("Positive","Negative"),
                    values = c("lightblue","red"))

# calculate the NRC sentiment scores
nrcScores <- data.frame()

magaScores <- get_nrc_sentiment(resultsUSmagaWords$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,data.frame(keyword="#maga",magaScores))

Chinascores <- get_nrc_sentiment(resultsUSchinaWords$word) %>%
  summarise_all(funs(mean))
nrcScores <- rbind(nrcScores,data.frame(keyword="China",Chinascores))


tallFormat = gather(nrcScores, key=emotion, value=score, anger:positive)

p <- ggplot(tallFormat,aes(x=keyword, y=score)) +
  geom_bar(stat = "identity",aes(fill = keyword)) +
  coord_flip() +
  ylab("Score of words") +
  xlab("words") +
  labs(title = "NRC sentiment analysis of tweets using US emojis", subtitle = "when people talk about China or #maga \nbetween 1:00 a.m. to 5a.m. Mar.28th, 2018", caption = "using syuzhet package")+
  scale_fill_manual(labels = c("#maga","China"), values = c('#8dafc6','#e08f93'), name = "Score of words")+plotTheme()

resultFacet <- p + facet_wrap(~emotion, ncol = 5)
resultFacet
