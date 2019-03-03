library("ggplot2")
library("stringi")
library("stringr")
library("data.table")
library(xlsx)
library(ggthemes)
library(gridExtra)
library(DataExplorer)


#Number of reviews for top brands.

dt <- fread("amzdata.csv",na.strings = c("", "NA"),stringsAsFactors = F) 
dt <- dt[complete.cases(dt),]
TopBrandsratedfive <- dt[tolower(`Brand Name`) %like% "apple|asus|blac|goog|htc|huw|nokia|lg|sam|sony"]

TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "apple","apple",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "asus","asus",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "blac","blackberry",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "goog","google",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "htc","htc",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "nokia","nokia",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "lg","lg",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "sam","samsung",`Brand Name`)]
TopBrandsratedfive <- TopBrandsratedfive[,`Brand Name` := ifelse(tolower(`Brand Name`) %like% "sony","sony",`Brand Name`)]
barplot(table(TopBrandsratedfive$`Brand Name`), col = "blue", main = "Number of Reviews By Brand"
        , ylab = "Counts", las=1)


Review_cnt <- amazon %>% group_by(Brand.Name) %>% dplyr::summarise(review_counts = n()) 
Top_Reviews <- Review_cnt %>% filter(review_counts >= 10000 & Brand.Name != '') %>% arrange(desc(review_counts)) 
head(Top_Reviews)
names(Top_Reviews) <-c("Brand","review_counts")
ggplot(Top_Reviews , aes(Brand, review_counts)) + geom_bar( stat="identity") + ggtitle("# of review counts by Brand") + theme_bw()
hchart(Top_Reviews, hcaes(x = Brand, y = review_counts, color = review_counts), type = "column") %>% 
  hc_credits(enabled = TRUE, text = "Source : ") %>%
  hc_add_theme(hc_theme_darkunica())  %>%
  hc_title(text = "# of review counts by Brand") %>%
  hc_subtitle(text = "")

Brand_Rating <- amazon %>% group_by(Brand.Name, Rating) %>% dplyr::summarise(review_counts = n()) 
names(Brand_Rating) <-c("Brand","Rating","review_counts")

Brand_Rating2 <- sqldf("select a.*, b.review_counts Total_counts from Brand_Rating a join Top_Reviews b on (a.Brand = b.Brand) ")
Brand_Rating2$Positive <- cut(Brand_Rating2$Rating, breaks=c(0,3,6), labels=c("Negative","Positive"))
head(Brand_Rating2)

str(Brand_Rating2)

ggplot(Brand_Rating2, aes(Brand , review_counts, fill=factor(Rating))) + geom_bar(stat="identity") + ggtitle("Rating Distribution by Brand") + theme_bw()
ggplot(Brand_Rating2, aes(Brand , review_counts, fill=factor(Positive))) + geom_bar(stat="identity") + ggtitle("Positive/Negative Distribution by Brand") + theme_bw()
ggplot(Brand_Rating2, aes(Brand , review_counts, fill=factor(Positive))) + geom_bar(stat="identity", position='fill') + ggtitle("Positive/Negative Distribution by Brand") + theme_bw()






#histogram for prices
qplot(amzdt$Price, geom = "histogram",
      binwidth = 30,
      main = "histogram for Price",
      xlab = "Price", ylab = "Count",
      xlim = c(0,1000),
      fill=I("blue"),
      col=I("red"),
      alpha=I(.3))



qplot(amzdt$Price, geom = "histogram",
      binwidth = 30,
      main = "histogram for Price",
      xlab = "Price", ylab = "Count",
      xlim = c(0,5),
      fill=I("blue"),
      col=I("red"),
      alpha=I(.3))







qplot(amzdt$Price, aes() geom = "boxplot")





#Ratings Count
qplot(Rating,data = dt, binwidth = 0.5, fill=I("red"), main = "Ratings count")


df <- amzdt$Price[[1000]]
barplot(df$Price)




# The character count of reviews
reviews<- amzdt$Reviews
revlen<- stri_length(reviews)
qplot(revlen, xlim = c(0,1500), ylim =c(0,2000),
      binwidth = 0.75,
      geom = "histogram",
      color = "dodgerblue2",
      main = "Review Length Distribution")




chopamzdt<- dt[sample(1:nrow(dt), 1400, replace= FALSE),]

qplot(chopamzdt$Price)
qplot(chopamzdt$Price, chopamzdt$Rating)









#Cleaning up the data for Correlation Test:
amazondata<- read.csv("amzdata.csv",na.strings = c("","NA"))
amzdt<- amazondata %>% na.omit()
meanp<-mean(amzdt$Price)
meanr<-mean(amzdt$Rating)
meanrv<-mean(amzdt$Review.Votes)
meanrr<-mean(revlen)
coramz<-cor.test(amzdt$Price, amzdt$Rating, method = "pearson")

attach(amzdt)
plot(Price, Rating, main="Scatterplot Example", 
     xlab="Prices", ylab="Ratings", pch=1)

















#FOR TEXT MINING
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("XML")
library("RCurl")

dm <-data.frame(dt)
dt.Corpus<-Corpus(VectorSource(dt$Reviews))
dt.Clean<-tm_map(dt.Corpus, PlainTextDocument)
dt.Clean<-tm_map(dt.Corpus,tolower)
dt.Clean<-tm_map(dt.Clean,removeNumbers)
dt.Clean<-tm_map(dt.Clean,removeWords,stopwords("english"))
dt.Clean<-tm_map(dt.Clean,removePunctuation)
dt.Clean<-tm_map(dt.Clean,stripWhitespace)
dt.Clean<-tm_map(dt.Clean,stemDocument)
dt.Clean<-tm_map(dt.Clean,removeWords,c("phone","screen","now","work","use"))

#Black Word Cloud Plot
wordcloud(dt.Clean,max.words = 200,random.color = TRUE,random.order=FALSE)
#Color Word Cloud Plot
wordcloud(words = dt.Clean, min.freq = 4000,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



# Reading the file for sentiment analysis
amazon <- read.csv(file.choose(), header = T)
#sampler <- amazon[sample(nrow(amazon), 1000), ]
sampler <- amazon[sample(1:nrow(amazon),5000, replace = FALSE),]

str(sampler)
corpus <- iconv(sampler$Reviews)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Cleaning the text for sentiment analysis
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*','', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
cleanset <- tm_map(cleanset, removeWords, c('phone', 'screen', 'battery','screen','use'))
cleanset <- tm_map(cleanset, stripWhitespace)
cleanset <- tm_map(cleanset, gsub, pattern = 'works', replacement = 'work')
inspect(corpus[1:5])

# NEW TDM
tdm <- TermDocumentMatrix(cleanset)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)



barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")



# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm
tdm[1:10, 1:20]

#Bar plot for sentiment analysis
w <- rowSums(tdm)
w <- subset(w, w>=200)
barplot(w, las = 2, col = rainbow(10))

#Word Cloud
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 400,
          random.order = FALSE,
          min.freq = 25,
          colors = brewer.pal(8, 'Dark2'),
          rot.per = 0.3)

#Sentiment Analysis
library(tidyselect)
library(tidytext)
library(magrittr)
library(dplyr)
library(sentimentr)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(sqldf)
library(highcharter)
library(viridisLite)
library(reshape)


amazon <- read.csv(file.choose(), header = T)
sampler <- amazon[sample(1:nrow(amazon),5000, replace = FALSE),]
reviews <- iconv(sampler$Reviews)
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
get_nrc_sentiment('software')
get_nrc_sentiment('good')
get_nrc_sentiment('break')
get_nrc_sentiment('Phone is great. I immediately rooted and flashed it with custom software. Got the device (used I think) with nary a mark or imperfection. Good job guys.')
barplot(colSums(s),
        las =2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Amazon Reviews')

#Sentiment on positive and negative
tdm %>%
  filter(n > 150) %>%
  mutate(n = ifelse(corpus == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

