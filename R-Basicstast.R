Cdn<-read.csv('cleanCdn.csv',head= T)
 

Cdn3<-Cdn

library(tm)
corpus2 <- iconv(Cdn3$description,"latin1","ASCII",sub ="")  
corpus2 <- Corpus(VectorSource(corpus2))
inspect(corpus2[1:4])


corpus <- iconv(Cdn$text,to= 'utf-8-mac')  

corpus <- Corpus(VectorSource(corpus))
Corpus()
inspect(corpus[1:4])

#  cleaning
corpus <- tm_map(corpus,tolower)


corpus <- tm_map(corpus,removePunctuation)

corpus <- tm_map(corpus,removeNumbers)

cleanset <- tm_map(corpus, removeWords,stopwords('english'))
inspect(cleanset[1:4])

removeURL <- function(x) gsub('http[[:alnum:]]*', '' , x)

cleanset <- tm_map(cleanset, content_transformer(removeURL))

cleanset <- tm_map(cleanset, stripWhitespace)

# saving the cleaned one
#Cdn2 <- data.frame(text=sapply(cleanset, identity),  stringsAsFactors=F)
#write.csv(Cdn2,'cleanCdn.csv') 

tdm <- TermDocumentMatrix(cleanset)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:10]

cleanset <- tm_map(cleanset, removeWords,c('cdnpoli','elxn','elxn43','say','says' )) # to remove irrelevant words!

#bar plot
w <- rowSums(tdm)
w<- subset(w, w>=5000) # least of repetition to show !
w
barplot(w, las=2,col=rainbow(50))

cleanset <-tm_map(cleanset,gsub, pattern='canadians', replacement='canadian')# synonms joining!
cleanset <-tm_map(cleanset,gsub, pattern='conservatives', replacement='conservative')
cleanset <-tm_map(cleanset,gsub, pattern='andrew', replacement='scheer')
cleanset <-tm_map(cleanset,gsub, pattern='justintrudeau', replacement='trudeau')
cleanset <-tm_map(cleanset,gsub, pattern='andrewscheer’s', replacement='scheer')
cleanset <-tm_map(cleanset,gsub, pattern='andrewscheer', replacement='scheer')
cleanset <-tm_map(cleanset,gsub, pattern='trudeaublackface', replacement='blackface')




# wordcloud

w <- sort(rowSums(tdm), decreasing = TRUE)

set.seed(222)

wordcloud(words=names(w), freq = w,max.words = 350,
          random.order = F,min.freq = 100, colors = brewer.pal(8,'Dark2'),
          scale=c(5,0.3),
          rot.per=0.2)

w <- data.frame(names(w),w)
write.csv(w,'w.csv')
colnames(w) <- c('word','freq')
wordcloud2(w,size=0.7,size=0.5,shape = "traingle", rotateRatio = 0.0, minSize = 10)


# reading again

elxn <- iconv(cleanCdn$text, to='utf-8-mac')
tweet <- iconv(Cdn$text,to= 'utf-8-mac')  

s <- get_nrc_sentiment(tweet)

head(s)
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab = 'count',
        main='sentiment of tweets')



