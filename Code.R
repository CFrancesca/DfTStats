#Author: Francesca Corvaglia

library(rtweet)
library(twitteR)
library(ggplot2)
library(pacman)
library(data.table)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(NLP)
library(lubridate)
library(syuzhet)
library(dplyr)
library(scales)
library(maps)
library(stringr)

#Scrape Twitter
# Set up account for Twitter Scraping
setwd("...TwitterDfTStats")
#twitteR
consumer_key <- "xxxx"
consumer_secret <- "xxxx"
access_token <- "xxxx"
access_secret <- "xxxx"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Rtweet
appname <- "xxxx"
## api key (example below is not a real key)
key <- "xxxx"
## api secret (example below is not a real key)
secret <- "xxxx"

# create token named "twitter_token"
twitter_token <- create_token( app = appname, consumer_key = key, consumer_secret = secret)

## save token to home directory
saveRDS(twitter_token, "Token/TweetDfT.rds")

#_________________________________________________________________________________________

#Get followers of Dft Stats Account 
dftstats <- getUser("dftstats")
#Get location of Dft Stats Account
location(dftstats)

#Get and remove protected account that brake the loop of the scraping using TwitteR
#Get followers of Dft Stats Account 
followerslist <- dftstats$getFollowers(retryOnRateLimit=180)
length(followerslist)
followersDF <- rbindlist(lapply(followerslist, as.data.frame))
#Remove protected followers of DftStats
followersDF <- followersDF[followersDF$protected==FALSE,]

#Get ID non protected users
followers <- followersDF$id

setwd("...TwitterDfTStats/TweetsFollowers")
for (i in 1:length(followers)) {
  #get last 1000 tweets of each DfT followers
  tweets <- get_timeline(followers[i], n = 1000)
  save_as_csv(tweets, followers[i])
  
  #Check rate limit version 1: if rate limit exhausted, then wait to rate limit reset
  #rl <- rate_limit(twitter_token, "statuses/user_timeline")
   #if (rl$remaining = 0L) {
  #Sys.sleep(as.numeric(rl$reset, "secs"))
  #}
  
  #Check rate limit version 2
  #Sys.sleep(as.numeric(30, "secs"))
}
#_________________________________________________________________________________________

#Clean and create the Database
setwd("...TwitterDfTStats")

#Import from .csv
#setwd("...TwitterDfTStats/TweetsFollowers")
#Import tweets of users and statistics of users
#tweetslist <- dir(pattern='.tweets')
#userslist<- dir(pattern='.users')

#Get list of csv for tweets and users
#tweets <- lapply(tweetslist, read.csv, header=TRUE, stringsAsFactors=FALSE)
#users <- lapply(userslist, read.csv, header=TRUE, stringsAsFactors=FALSE)

#Convert list to DF
#tweetsDF <- as.data.frame(rbindlist(tweets))
#usersDF <- as.data.frame(rbindlist(users))

#Combine tweets with users statistics
#tweetsDB <- merge(tweetsDF, usersDF, by = "user_id" , all.x = TRUE)
#setnames(tweetsDB, 'screen_name.x', 'screen_name')

setwd("...TwitterDfTStats")
#write.csv(tweetsDB, "Tweets210118.csv")
#_________________________________________________________________________________________
#Load Database and remove all the Tweets not related to DfTStats and obtain a smaller DB
tweetsDB <- read.csv("Tweets210118.csv", header=TRUE, stringsAsFactors=FALSE)
columns <- c("status_id","user_id","screen_name", "name", "location", "text", "created_at",
             "description", "reply_to_user_id", "reply_to_screen_name", "is_quote", "is_retweet", 
             "mentions_user_id", "mentions_screen_name", "favorite_count", "retweet_count",
             "hashtags", "urls_url", "urls_t.co", "urls_expanded_url", "retweet_status_id", 
             "retweet_text","followers_count", "friends_count", "listed_count", "statuses_count", 
             "favourites_count", "account_created_at")

tweetsF <- tweetsDB[, which(names(tweetsDB) %in% columns)]
tweetsF <- tweetsF[, columns]

twDfTstats <-tweetsF[which(grepl( "DfTstats", tweetsF$text )), ] 
#write.csv(twDfTstats, "Tweets210118DfTstats.csv")
#_________________________________________________________________________________________
setwd("...TwitterDfTStats")
twDfTstats <- read.csv("Tweets210118DfTstats.csv", header=TRUE, stringsAsFactors=FALSE)

#Analysis
#Number of Tweets collected for DfTstats
length(twDfTstats$text)

#Descriptive Statistics on Tweets
# characters per tweet
textDft <- as.vector(twDfTstats$text)
textDB <- as.vector(tweetsDB$text)
chars_per_tweetDfT = sapply(textDft, nchar)
chars_per_tweetDB = sapply(textDB, nchar)
summary(chars_per_tweetDfT)
summary(chars_per_tweetDB)

# words per tweets
# split words
words_list = strsplit(textDft, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet),
        main="Distribution of number words per tweet", cex.main=1)

# Unique words per tweets
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
# barplot
barplot(table(uniq_words_per_tweet),
        main="Distribution of unique words per tweet", cex.main=1)

# how many hashtags per tweet
hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
table(hash_per_tweet)
prop.table(table(hash_per_tweet))
barplot(table(hash_per_tweet))

# how many @mentions per tweet
ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))
barplot(table(ats_per_tweet))

# most frequent words
mfw = sort(table(tolower(unlist(words_list))), decreasing=TRUE)

# to most frequent
top_word = head(mfw, 200)
names(top_word)
top_word <- top_word[which(names(top_word) %in% 
  c("road", "stats", "transport", "#roadsafety", "statistics", "deaths", "helicopter", "rescue",
    "england", "#sarh", "#tsgb2016", "million", "#nationaltravelsurvey", "#tsgb2017", "june",
    "travel", "vehicles", "provisional", "areas", "#roadfreight", "average", "passenger", "publication",
    "#roadstats", "april", "march", "registered", "figures", "growth", "september", "accidents",
    "data", "journeys", "people", "published", "practical", "theory", "estimates", "local",
    "tonnes", "#bus", "#roadtraffic", "annual", "bus", "car", "december", "freight", "hgvs", 
    "@transportgovuk", "billion", "interactive", "2,594", "check",  "results", "team",
    "#drivingtests", "roads", "traffic", "#walking", "2%", "map", "national", "public",                  
    "reported", "driving", "official", "record", "survey", "#maritime", "#ulev", "fuel",                   
    "rail", "release", "#roadcondition", "1,792", "2017", "5", "concessionary", "fatalities",                             
    "major", "passengers", "safety", "travelled", "travelling", "#buses", "#cycling", "#drinkdrive",             
    "conditions"))]

# barplot
barplot(top_word, border=NA, las=2, main="Top 200 most frequent terms", cex.main=1)
as.data.frame(top_word)

par(bg = "gray95")
wordcloud(names(top_word), (as.vector(top_word)), scale=c(3.2,0.1), max.words=150, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors = c("aquamarine4", "azure4", "black"))

#Lets look at when they were created:
twDfTstats$datetime <- ymd_hms(twDfTstats$created)
#Order by date
twDfTstats <- twDfTstats[order(twDfTstats$datetime),]
row.names(twDfTstats) <- c()
# By Twitter Timeline
twDfTstats$ym <- substr(twDfTstats$datetime, 1, 7)
twDfTstats$ym <- as.Date(paste0(twDfTstats$ym, "-01", format = "%Y-%m-%d"))

ggplot(data = twDfTstats, aes(x = ym)) +
  geom_histogram(aes(fill = ..count..), bins = 23) +
  ggtitle("Tweets on DfTStats in the last 2 years") +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "gray30", high = "aquamarine4")

#By Year
ggplot(data = twDfTstats, aes(x = year(datetime))) +
  geom_bar(aes(fill = ..count..)) +
  ggtitle("Tweets on DfTStats by year") +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "gray30", high = "aquamarine4")

#By Month
ggplot(data = twDfTstats, aes(x = month(datetime, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  ggtitle("Tweets on DfTStats by month") +
  theme(legend.position = "none") +
  xlab("Month") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "gray30", high = "aquamarine4")

#By Week Day
ggplot(data = twDfTstats, aes(x = wday(datetime, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  ggtitle("Tweets on DfTStats by day of the week") +
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "gray30", high = "aquamarine4")

#By Time Day
ggplot(data = twDfTstats, aes(x = hour(datetime))) +
  geom_bar(aes(fill = ..count..)) +
  ggtitle("Tweets on DfTStats by time of the day") +
  theme(legend.position = "none") +
  xlab("time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "gray30", high = "aquamarine4")


#Sentiment Analysis
mySentiment <- get_nrc_sentiment(twDfTstats$text)
twDfTstats <- cbind(twDfTstats, mySentiment)
sentimentTotals <- data.frame(colSums(mySentiment[,1:8]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

twDfTstats$month <- month(twDfTstats$datetime, label = TRUE)
monthlysentiment <- twDfTstats %>% group_by(month) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% melt
names(monthlysentiment) <- c("month", "sentiment", "meanvalue")

ggplot(data = monthlysentiment, aes(x = month, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment During the Year")

twDfTstats$datetime <- with_tz(ymd_hms(twDfTstats$datetime))
posnegtime <- twDfTstats %>% 
  group_by(datetime = cut(datetime, breaks="2 months")) %>%
  summarise(negative = mean(negative),
            positive = mean(positive)) %>% melt
names(posnegtime) <- c("datetime", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])

ggplot(data = posnegtime, aes(x = as.Date(datetime), y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) + 
  scale_colour_manual(values = c("springgreen4", "firebrick3")) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  scale_x_date(breaks = date_breaks("9 months"), 
               labels = date_format("%Y-%b")) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment Over Time")


#Export Location to clean
#Location <- twDfTstats[, which(names(twDfTstats) %in% c("user_id","location"))]
#Location <- Location[!duplicated(Location$user_id), ]
#write.csv(Location, "Unique location.csv", row.names = FALSE) 
#city <- read.csv("Unique location.csv", header=TRUE, stringsAsFactors = FALSE)
#city <- city[complete.cases(city$City), ]
#register_google(key = "xxxx")
#for(i in 1:nrow(city)){
#  result <- geocode(city$City[i], output = "latlona" , source = "google")
#  city$lon[i] <- as.numeric(result[1])
#  city$lat[i] <- as.numeric(result[2])
#  city$geoAddress[i] <- as.character(result[3])
#}
#write.csv(city, "Unique location.csv", row.names = FALSE) 
city <- read.csv("Unique location.csv", header=TRUE, stringsAsFactors = FALSE)
twDfTstats2 <- merge(twDfTstats, city, by= "user_id", all.x = TRUE)
twDfTstats2 <- twDfTstats2[complete.cases(twDfTstats2$City), ]

map("world",fill=TRUE,col=0,xlim=c(-8,4),ylim=c(50,59))
with(twDfTstats2, points(lon, lat, pch = 20, cex = 3, col = rgb(0, .4, .15, .45)))

