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
library(dplyr)
library(repmis)

## Con repmis se pueden poder varios working directories para no tener que cambiarlo siempre
possible_dir <- c('C:/Users/maria_000/Documents/GitHub/Fightfor15', '/Users/mariorodriguez/Desktop/Hertie/3rd Semester/PolComNewMedia/Fightfor15')
repmis::set_valid_wd(possible_dir)

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
wordcloud(hashtags_df[2:50,1], hashtags_df[2:50,2], random.order=F, colors=brewer.pal(8, "Dark2"), scale = c(5,0.5))
wordcloud(hashtags_df[2:50,1], hashtags_df[2:50,2], random.order=F, colors=colorRampPalette(brewer.pal(8, "Dark2"))(32), scale = c(5,0.5))


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


###      Filtering cual es contenido original? dentro de ese, cual si se esta propagando, retweetCount>10   ###
originalAll<- filter(TweetsMinWage100k, isRetweet == FALSE)

TweetsMinWageFiltered <- filter(TweetsMinWage100k, retweetCount >=10 )
original<-filter(TweetsMinWageFiltered, isRetweet == FALSE)

### Conversations - who is replying and engaging in a conversation ###
conversation<- filter(TweetsMinWage100k, !is.na(TweetsMinWage100k$replyToSN))
pplconversing <- unique(TweetsMinWage100k$replyToSN)


### Growth of network use over time ###
TweetsMinWage100k$time <-as.POSIXlt(TweetsMinWage100k$created)

### Nodes with measures from gephi ###
nodes<- read.csv("Yifan Hu/YifanHu model - Workspace 3 [Nodes].csv")
nodes_indegree<- nodes[order(nodes$indegree, decreasing = T), ]
nodes_indegree<- nodes_indegree[1:100,]
write.csv(nodes_indegree, file = "nodes_indegree.csv")

nodes_outdegree <- nodes[order(nodes$outdegree, decreasing = T),]
nodes_outdegree <- nodes_outdegree[1:100,]
write.csv(nodes_outdegree, file = "nodes_outdegree.csv")

nodes_betweeness <- nodes[order(nodes$betweenesscentrality, decreasing = T),]
nodes_betweeness<- nodes_betweeness[1:100,]
write.csv(nodes_betweeness, file = "nodes_betweeness.csv")
nodes_betweeness <- select(nodes_betweeness, id, betweenesscentrality)


nodes_indegree<- read.csv("nodes_indegree_classified.csv")
nodes_indegree$id <- as.character(nodes_indegree$id)

nodes_in_between<-left_join(nodes_betweeness, nodes_indegree, by = "id")
write.csv(nodes_in_between, file = "nodes_in_between.csv")

nodes_between_classified<- read.csv("nodes_betweenINclassified.csv")

### most retweeted
mostRetweeted <- TweetsMinWage100k[order(TweetsMinWage100k$retweetCount, decreasing = T),]
mostRetweeted<-select(mostRetweeted, screenName, retweetCount)
most
mostRetweeted <- TweetsMinWage100k[which(unique(TweetsMinWage100k$text)),]

### recombining databases with classification ###
nodes_between_classified<- select(nodes_between_classified, id, Type)
nodes_indegree<-select(nodes_indegree, id, Type, indegree)
nodeNames_in<-nodes_indegree$id %>% as.character()
nodesNames_between<- nodes_between_classified$id %>% as.character()

nodes_type<-full_join(nodes_between_classified, nodes_indegree)

Names_brokers_receivers<- c(nodeNames_in, nodesNames_between)
Names_brokers_receivers<-unique(Names_brokers_receivers)

nodes<-select(nodes, id, indegree, eccentricity, modularity_class, betweenesscentrality, degree, closnesscentrality)

key_players<-full_join(nodes, nodes_type)
key_players<-filter(key_players, !is.na(key_players$Type))
key_players$Type<- as.factor(key_players$Type)


key_players$Type2 <-recode(key_players$Type, "1" = "traditional organizations",
                                 "2" = "politicians",
                                 "3" = "newspapers/media",
                                 "4" = "citizens",
                                 "5" = "opinion leaders/influencers",
                                 "6" = "advocacy groups")

summary(key_players$Type2)
