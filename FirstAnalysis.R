library(ROAuth)
library(twitteR)
library(streamR)
library(RCurl)
library(maps)
library(devtools)
library(tm)
library(Rfacebook)
library(base64enc)
library(ROAuth)
library(SnowballC)
library(igraph)
library(ggplot2)
library(stringr)
library(wordcloud)

### Recuerden cambiar el working directory por el suyo para que les corra el script
setwd('/Users/mariorodriguez/Desktop/Hertie/3rd Semester/PolComNewMedia/Fightfor15')

#### Este sólo es el análisis que nos dio el profesor. Lo bueno es que pudimos hacer todos los comandos que nos enseñó. Bernie Sanders es por mucho el más retwiteado

#### Más retweets

TweetsMinWage100k <- read.csv("TweetsMinWage100k.csv")

retweet_users <- gsub("(RT @|via @)", "", str_extract(TweetsMinWage100k$text, "(RT|via)((?:\\b\\W*@\\w+))"))  #check data 
retweet_users_table <- table(retweet_users)
retweet_df <- as.data.frame(retweet_users_table, stringsAsFactors = F)

retweet_df

order(retweet_df$Freq, decreasing = T)

retweet_df <- retweet_df[order(retweet_df$Freq, decreasing = T), ]

retweet_top_df <- retweet_df[1:20,]

ggplot() + geom_bar(data=retweet_top_df, aes(x = reorder(retweet_users, Freq), y= Freq), stat="identity") +
  theme_bw()+xlab(NULL) + 
  theme(axis.text.x=element_text(angle=90,size=14))

#### Hashtag cloud

Fightfor15Text <- TweetsMinWage100k[,c("text","screenName")]

Fightfor15Text[,"text"] <- str_replace_all(Fightfor15Text$text,"[^[:graph:]]", " ") 

Fightfor15Text[,"text"] <- tolower(Fightfor15Text$text)

hashtagsFightfor15 <- str_extract_all(Fightfor15Text$text, "#\\w+")

hashtagsFightfor15 <- unlist(hashtagsFightfor15)
hashtags_df <- table(hashtagsFightfor15)
hashtags_df <- as.data.frame(hashtags_df)
hashtags_df <- hashtags_df[order(-(hashtags_df[,2])),]

wordcloud(hashtags_df[1:50,1], hashtags_df[1:50,2], random.order=F) 


###Removing the first hashtag
wordcloud(hashtags_df[2:50,1], hashtags_df[2:50,2], random.order=F)
wordcloud(hashtags_df[2:50,1], hashtags_df[2:50,2], random.order=F, colors=brewer.pal(8, "Dark2"))

png("wordcloud.png", width=12, height=8, units="in", res=300)
wordcloud(hashtags_df[2:50,1], hashtags_df[2:50,2], random.order=F, colors=brewer.pal(8, "Dark2"))
dev.off()

####Network Analysis

tw <- read.csv("TweetsMinWage100k.csv")

tw <- tw[grepl('RT @', tw$text),]

edges <- data.frame(
  retweeter = tw$screenName,
  retweeted = gsub('.*RT @([a-zA-Z0-9_]+):? ?.*', tw$text, repl="\\1"),
  stringsAsFactors=F)

View(edges)

write.csv(edges, file = "EdgesFightfor15.csv")

graphObject = as.matrix(read.csv("EdgesFightfor15.csv", sep=",", header = FALSE))

Fightfor15graph<-graph_from_edgelist(graphObject, directed = TRUE)

is.connected(Fightfor15graph)

vcount(Fightfor15graph)

ecount(Fightfor15graph)

diameter(Fightfor15graph, weights = NA)

summary(degree(Fightfor15graph))

summary(betweenness(Fightfor15graph))

summary(degree(Fightfor15graph, mode="in"))

summary(degree(Fightfor15graph, mode="out"))

degree(Fightfor15graph)

infoFlOWers<-edge.betweenness(Fightfor15graph)

E(Fightfor15graph)[order(infoFlOWers, decreasing=T)[1:10]]

Fightfor15DegreePlot<-degree(Fightfor15graph)

hist(Fightfor15DegreePlot)

hist(Fightfor15DegreePlot[Fightfor15DegreePlot<=20],col="blue",
     xlab="Degree", ylab="Frequency",
     main="Degree Distribution of #Fightfor15 network", breaks=1:20)

Fightfor15BetweenPlot<-betweenness(Fightfor15graph)

hist(Fightfor15BetweenPlot)

reciprocity(Fightfor15graph, ignore.loops = TRUE, mode = c("default"))

table(sapply(cliques(Fightfor15graph),length)) ###Este se tardó muchisimo en procesar, pero lo logré despuès de como media hora

plot(Fightfor15graph, layout = layout.kamada.kawai)  ###Este nada más no lo pude correr, igual alguien con un procesador más nuevo 


