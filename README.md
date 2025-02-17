## US vs China in Twitter Sentiment Analysis

#### Sentimental Analysis of Emoji “US” in Twitter based on tweets searched from keywords "China" and "#maga"

<p align="center">
<img src = "results/Emoji%20Frequency%20Comparison.jpeg" width = "700">
</p>

This is a sentimental analysis for Tweets with emojis. In this analysis, I chose two keywords, “China” and “#maga” to collect tweets. These two words have similar uses but might have different sentimental conditions. I hope to see what emojis people usually use when they are talking about these two topics, which emoji is used the most often in both topics and if they represent similar sentimental conditions.
<br>
This analysis took three steps. The first step is to collect tweets using the SteamR and the ROAuth package. I used a total number of 50000 for the streaming API and the final results returned for both of the keywords was around 13000. After filtering out the retweets, there were about 6000 left totally. The second step is to filter out the tweets that used emojis and the total number of appearances for each of them. Then from the most frequently used emojis, I selected the emoji, “REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S”(US), which appears a lot in both keywords to do the sentimental analysis.
<br>
There are many challenges in creating this map. First, these keywords are not the most frequently used ones on twitter, so it took pretty long time collecting the tweets. Second, because it is time-consuming to collect the tweets people sent out, I usually do it at night, and the collections of tweets sometimes contain many other languages instead of English. The third challenge is my topic is the sentimental analysis of one specific emoji, the dataset was filtered smaller and smaller. Therefore, the analysis results only represent the sentimental condition of people who tweeted about China or #mage between March 27th 2am-6am in the Eastern Time Zone. (_I also added one more dataset I collected March 28th 1am-5am in the Eastern Time Zone, the json file could be downloaded [here.](https://drive.google.com/drive/folders/1qpJLyI8ZhNTQ3UrcuqW87Px56hsb1cLx?usp=sharing)_)
<br>
The [results](results) of this analysis is divided into three halves. From the first result, [Emoji frequency comparison](results/Emoji%20Frequency%20Comparison.jpeg), we can see that when people talk about China, they use “REGIONAL INDICATOR SYMBOL LETTER C + REGIONAL INDICATOR SYMBOL LETTER N”(CN) the most, which makes a lot of sense. They also used “FACE WITH TEARS OF JOY” and “REGIONAL INDICATOR SYMBOL LETTER U + REGIONAL INDICATOR SYMBOL LETTER S”(US) a lot. And when people talk about #maga, what they use the most was the "US", “FACE WITH TEARS OF JOY” and “HEAVY BLACK HEART”. The "US" is more interesting to me, so I filtered out the tweets in both keywords using emoji "US". I also applied a word frequency test on both of these datasets. What is really interesting is "trump" appeared a lot in both the tweets returned from [keyword “China”](results/Word%20Frequency%20of%20tweets%20talking%20about%20China%20with%20US%20emoji.jpeg) and [keyword #maga](results/Word%20Frequency%20of%20tweets%20talking%20about%20%23maga%20with%20US%20emoji.jpeg).
<br>

<p align="center">
<img src = "results/Word%20Frequency%20of%20tweets%20talking%20about%20China%20with%20US%20emoji.jpeg" width = "600">
</p>


<p align="center">
<img src = "results/Word%20Frequency%20of%20tweets%20talking%20about%20%23maga%20with%20US%20emoji.jpeg" width = "600">
</p>

<br>
[The final results of sentimental analysis of US emoji](results/Sentiment%20analysis%20of%20tweets%20using%20US%20emoji-02-06-0327.jpeg) showed that when people tweet about China, they seemed to be generally in a negative mood with an overall mean score of -0.03, but when they tweeted about #maga, they seemed to be in a much more positive mood with an overall mean score about 0.04.(_After I added the other dataset I collected March 28th 1am-5am in the Eastern Time Zone, [the results](results/Sentiment%20analysis%20of%20tweets%20using%20US%20emoji-01-05-0328.jpeg) both seem positive, but keyword "#maga" still looks more positive than keyword "China"._) 
<br>

<p align="center">
<img src = "results/Sentiment%20analysis%20of%20tweets%20using%20US%20emoji-02-06-0327.jpeg" width = "450">
</p>

<br>
[the result of sentimental analysis using NRC](results/NRC%20sentiment%20analysis%20results-02-06-0327.jpeg) shows that generally all the top words commonly used in both of the topics, China had slightly higher score than #maga only on “negative”. That also indicated that when people tweet with emoji "US" about China, they seem to be in a relatively more negative sentimental condition than when they tweeted about #maga (_After adding the new dataset [the results](results/Sentiment%20analysis%20of%20tweets%20using%20US%20emoji-01-05-0328.jpeg) still seem the same_).
<br>
<p align="center">
<img src = "results/NRC%20sentiment%20analysis%20results-01-05-0328.jpeg" width = "600">
</p>
