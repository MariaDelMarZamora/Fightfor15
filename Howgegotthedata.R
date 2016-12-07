library(ROAuth)
library(twitteR)
library(streamR)
library(stringr)
library(RCurl)
library(maps)
library(devtools)
library(tm)
library(Rfacebook)
library(base64enc)
library(ROAuth)
library(SnowballC)
library(igraph)

setwd('/Users/mariorodriguez/Desktop/Hertie/3rd Semester/PolComNewMedia')

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "1YbhyroEaYAuzPoKey7iCLBBL"
consumerSecret <- "EBc0proP8Jh1aRxIES9QyGwS2CIMAu5Jc3zrDLQDKIz5y3DM5B"

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")) 

accessToken = '735118568120025088-E1MS3g3tyWG6Qba4ltcZ94mJZFQXwoz'
accessSecret = 'BcB6QTmtkIrQhoPhWBjWeLmpxt1KTV5gKfX4c1AumxZS4'

setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret,
                    access_token=accessToken, access_secret=accessSecret)
searchTwitter('Trump', n=1)

TweetsMinWage <- searchTwitter('#Fightfor15', n=300)

TweetsMinWage <- twListToDF(TweetsMinWage)

View(TweetsMinWage)

write.csv(TweetsMinWage, "TweetsMinWage.csv")

BrexitTweets$created

TweetsDog <-searchTwitter('#buscandoaMika', n=10000)

TweetsDog <- twListToDF(TweetsDog)

write.csv(TweetsDog, "Tweetsdog.csv")

View(TweetsDog)

filterStream(file.name="fightfor15.json", track="#fightfor15", 
             timeout=57600, oauth=my_oauth)

Fightfor15Tweets <- parseTweets("fightfor15.json")

View(Fightfor15Tweets)

write.csv(Fightfor15Tweets, file = "fightfor15.csv")

TweetsMinWage <- searchTwitter('#Fightfor15', n=50000)

TweetsMinWage <- twListToDF(TweetsMinWage)

write.csv(TweetsMinWage, "TweetsMinWage.csv")

TweetsMinWage100k <- searchTwitter('#Fightfor15', n=100000)

TweetsMinWage100k <- twListToDF(TweetsMinWage100k)

write.csv(TweetsMinWage100k, "TweetsMinWage100k.csv")

TweetsMinWage100k <- read.csv("TweetsMinWage100k.csv")

summary(TweetsMinWage100k$retweetCount)

TweetsMinWage100k$text[which.max(TweetsMinWage100k$retweetCount)]