library (twitteR)
library (ROAuth)
library (rtweet)
library (tm)
library (wordcloud)
library (tidytext)
library (syuzhet)
library (textdata)
library (ggplot2)
library (igraph)
library (stringr)
library (readr)

#chiavi di accesso
consumer_key <- "crTSIxU1KOvRPCSKtucCqv1j1"
consumer_secret <- "rJfoo8w4vpRCwxe8KeI4sYk4Th7UVwJT0hIzK44qcklNzq2Pfk"
access_token <- "852981572-bFJ9VYUWkJmyhBMkA7sHKfThFaB9TBlOhUjgYYie"
access_secret <- "cwmzQadtUkKBwwNJIFbx5JjMpRaaPBKZt0a9g2gsh3JpV"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

#acquisizione dati
hashtag <- c('#Venezia77')
tweets_ls <- searchTwitter(hashtag, n=400, since="2020-09-02", until="2020-09-12", lang="en", resultType="mixed")
tweets_df <- do.call(rbind, lapply(tweets_ls, as.data.frame))
tweets_df_isretweet <- tweets_df[tweets_df$isRetweet==TRUE,]
r_st <- "D:/Daniela/UNIVERSITA'/MAGES/SECONDO ANNO/Secondo semestre/Esami/SOCIAL MEDIA ANALYTICS/Progetto/R-studio progetto/"
rt_patterns <- 1:dim(tweets_df_isretweet)[1]
who_retweet <- as.list(1:length(rt_patterns))
who_post <- as.list(1:length(rt_patterns))

for (i in 1:length(rt_patterns)){
  tw = tweets_ls[[rt_patterns[i]]]
  poster = str_extract_all(tw$getText(),"(RT|via)((?:\\b\\W*@\\w+)+)")
  poster = gsub(":", "", unlist(poster))
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE)
  who_retweet[[i]] = rep(tw$getScreenName(), length(poster))
}

who_retweet_df <- do.call(rbind, lapply(who_retweet, as.data.frame))
who_post_df <- do.call(rbind, lapply(who_post, as.data.frame))
people_df <- rbind(who_retweet_df, who_post_df)
people_df <- unique(people_df)
write.table(people_df, paste(r_st,"people.csv"),quote=F, sep=";", dec=",", na="", row.names=F, col.names=T)

who_post <- unlist(who_post)
who_retweet <- unlist(who_retweet)

rt_tw <- cbind(who_retweet, who_post)
rt_graph <- graph.edgelist(rt_tw)
glay <- layout_with_kk(rt_graph)

#Plotta il Network Graph con il layout Kamada-Kawai algorithm
plot(rt_graph, layout=glay,
     vertex.color="black", #colore dei nodi
     vertex.size=8, #dimensione dei nodi
     vertex.label.family="sans", #font senza grazie
     vertex.shape="none", #nessuna forma dei nodi
     vertex.label.color=hsv(h=.165, s=.28, v=.08, alpha=1),
     vertex.label.cex=0.85, #dimensione del font
     edge.arrow.size=0.8, #dimensione delel frecce
     edge.arrow.width=0.5, #larghezza delle frecce
     edge.width=3, #larghezza degli archi
     edge.color=hsv(h=0, s=1, v=1, alpha=1)) #colore degli archi
title("Tweets about #Venezia77: Who retweets whom")

rt_tw_df <- as.data.frame.matrix(rt_tw)
write.table(rt_tw_df, paste(r_st,"tweet_retweet_matrix.csv"),quote=F, sep=";", dec=",", na="", row.names=F, col.names=T)

#grafico tweet-retweet
Venezia77Network <- graph_from_data_frame(d=rt_tw_df, directed=TRUE,vertices=people_df)
#plot(Venezia77Network)
plot(Venezia77Network, layout=glay,
     vertex.color="blue", #colore dei nodi
     vertex.size=2, #dimensione dei nodi
     vertex.label.family="sans", #font senza grazie
     vertex.label.color=hsv(h=.165, s=.28, v=.08, alpha=1),
     vertex.label.cex=0.3, #dimensione del font
     edge.arrow.size=0.3, #dimensione delle frecce
     edge.arrow.width=0.2, #larghezza delle frecce
     edge.width=0.2, #larghezza degli archi
     edge.color=hsv(h=0, s=1, v=1, alpha=1))

degree(Venezia77Network)
betweenness(Venezia77Network, normalized=T)
distances(Venezia77Network, v=V(Venezia77Network)["la_Biennale"],to=V(Venezia77Network), weights=NA)

degree_df <- do.call(rbind, lapply(degree(Venezia77Network), as.data.frame))
distances_df <- do.call(rbind, lapply(distances(Venezia77Network, v=V(Venezia77Network)["la_Biennale"],to=V(Venezia77Network), weights=NA), as.data.frame))
betweenneess_df <- do.call(rbind, lapply(betweenness(Venezia77Network, normalized=T), as.data.frame))
write.table(degree_df, paste(r_st,"degree.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)
write.table(distances_df, paste(r_st,"distances.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)
write.table(distances_df, paste(r_st,"betweenness.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)

#salvataggio dati e confronto T e RT
write.table(tweets_df, paste(r_st,"tweetsVenezia.csv"),quote=F, sep=";", dec=",", na="", row.names=F, col.names=T)

#Pulizia dati
bigdata_list <- sapply(tweets_ls, function(x) x$getText())
bigdata_df <- do.call(rbind, lapply(bigdata_list, as.data.frame))
bigdata_corpus <- Corpus(VectorSource(bigdata_list))
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
bigdata_corpus <- tm_map(bigdata_corpus, removeURL)
bigdata_corpus <- tm_map(bigdata_corpus, content_transformer(gsub), pattern="\\W",replace=" ")
bigdata_corpus <- tm_map(bigdata_corpus,function(x)removeWords(x,stopwords(kind="en")))
bigdata_corpus <- tm_map(bigdata_corpus, removePunctuation)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
bigdata_corpus <- tm_map(bigdata_corpus, toSpace, "/")
bigdata_corpus <- tm_map(bigdata_corpus, toSpace, "\\|")
bigdata_corpus <- tm_map(bigdata_corpus, toSpace, "@")
dataCleared_df <- data.frame(text = sapply(bigdata_corpus, as.character), stringsAsFactors = FALSE)
write.table(dataCleared_df, paste(r_st,"tweetsVenezia_cleared.csv"),quote=F, sep=";", dec=",", na="", row.names=F, col.names=T)


#Analisi dati

#wordcloud
wordcloud(bigdata_corpus,min.freq=5)

#matrice di frequenza 
bigdata_tdm <- TermDocumentMatrix(bigdata_corpus)
findFreqTerms(bigdata_tdm, lowfreq=10, highfreq=Inf)
#Matrice singoli termini
m <- as.matrix(bigdata_tdm)
v <- sort(rowSums(m), decreasing=TRUE)
N <- 15
n <- as.matrix(head(v,N))
n_df <- as.data.frame.matrix(n)
write.table(n_df, paste(r_st,"most_frequent_words.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=F)

assoc_terms_1 <- findAssocs(bigdata_tdm, 'venezia77', 0.3)
assoc_terms_1df <- do.call(rbind, lapply(assoc_terms_1, as.data.frame))
write.table(assoc_terms_1df, paste(r_st,"assoc_terms_1.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=F)
assoc_terms_2 <- findAssocs(bigdata_tdm, 'biennalecinema2020', 0.3)
assoc_terms_2df <- do.call(rbind, lapply(assoc_terms_2, as.data.frame))
write.table(assoc_terms_2df, paste(r_st,"assoc_terms_2.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=F)
assoc_terms_3 <- findAssocs(bigdata_tdm, 'cateblanchett', 0.3)
assoc_terms_3df <- do.call(rbind, lapply(assoc_terms_3, as.data.frame))
write.table(assoc_terms_3df, paste(r_st,"assoc_terms_3.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=F)

#sentiment analysis
get_sentiments("nrc")
get_sentiments("bing")
get_sentiments("afinn")

result <- get_nrc_sentiment (as.character (bigdata_tdm$dimnames))
result1 <- data.frame (t(result))
new_result <- data.frame (rowSums (result1))
names(new_result)[1] <- "count"
new_result <- cbind("sentiment"=rownames(new_result),new_result)
result <- get_nrc_sentiment(as.character(bigdata_tdm$dimnames))
result1<-data.frame(t(result))
new_result <- data.frame(rowSums(result1))
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL
new_result_df <- data.frame(text = sapply(new_result, as.character), stringsAsFactors = FALSE)
write.table(new_result_df, paste(r_st,"new_result.csv"),quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)

qplot(sentiment , data=new_result [1:8,], weight=count,geom="bar",fill = sentiment) +ggtitle ("Venezia77 Emotions")

qplot(sentiment , data=new_result [9:10,], weight=count,geom="bar",fill = sentiment) +ggtitle ("Venezia77 Emotions")