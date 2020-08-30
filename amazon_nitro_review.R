library(rvest)
library(XML)
library(magrittr)
##extracting the reviews of the acer nitro 5 laptop product from amazon
aurl <- "https://www.amazon.in/Acer-AN515-52-15-6-inch-i5-8300H-Graphics/dp/B07W6H3FM3/ref=sr_1_1?dchild=1&keywords=nitro+5&qid=1587100743&sr=8-1"
amazon_reviews <- NULL
for (i in 1:20) {
  murl <- read_html(as.character(paste(aurl,i,sep = "=")))
  rev <- murl%>%
    html_nodes(".review-text")%>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
amazon_reviews
write.csv(amazon_reviews,file = "nitro reviews.csv")
getwd()
###  
acer_nitro_amazonreview <- read.csv("D://DATA SCIENCE//PRATICE R//nitro reviews.csv")
nitro <- acer_nitro_amazonreview[,2]
View(nitro)
##creating corpus data
library(tm)
corpus <-Corpus(VectorSource(nitro)) 
class(corpus)
View(corpus)
##cleaning the data (cc-corpusclean)
cc <- tm_map(corpus,content_transformer(tolower))
inspect(cc[1:5])

cc <- tm_map(cc,removePunctuation)
inspect(cc[1:5])

cc <- tm_map(cc,removeWords,stopwords("en"))
inspect(cc[1:5])

cc <- tm_map(cc,removeWords,c("acer","laptops","laptop"))
inspect(cc[1:5])

cc <- tm_map(cc,stripWhitespace)
inspect(cc[1:5])

##document term matrix
dtm <- DocumentTermMatrix(cc)
dtm
dtm <- as.matrix(dtm)
dtm[1:10,1:10]
##barplot
w <- colSums(dtm)
w <- subset(w,w>50)
barplot(w,
        las=1,
        col=rainbow(50))
##wordcloud
library(wordcloud)
w <- sort(colSums(dtm),decreasing = TRUE) 
set.seed(200)
wordcloud(words = names(w),
          freq = w,
          random.order = F,
          colors=brewer.pal(8,"Dark2"),
          scale=c(5,0.3))

library(wordcloud2)
w2 <- data.frame(names(w),w) 
View(w2)
colnames(w2) <- c("word","freq")
wordcloud2(w2,
           size =0.5,
           shape="circle",
           rotateRatio = 0.5,
           minSize = 0)
## sentimental analysis
library(syuzhet)
library(lubridate)  
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

##
acer_nitro_amazonreview <- read.csv("D://DATA SCIENCE//PRATICE R//nitro reviews.csv")
nitro <- iconv(acer_nitro_amazonreview$x)
View(nitro)
s <- get_nrc_sentiment(nitro)
head(s)
##barplot
barplot(colSums(s),
                las= 2,
                col= rainbow(10),
                ylab = "count",
                main= " SENTIMENT SCORES FOR NITRO REVIEWS")
#From above sentiment analysis of the 
##amazon reviews of the acer nitro 5 laptop are positive and trust based comments.. 