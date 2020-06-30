#Read Trump Twitter Archive
#https://dzone.com/articles/analyzing-trumps-android-and-iphone-tweets-one-yea
#install.packages('tidyverse')
library(tidyverse)
library(lubridate)
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
all_tweets <- map(2009:2018, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  mutate(created_at = parse_date_time(created_at, "a b! d! H!:M!:S! z!* Y!")) %>%
  arrange(created_at) %>%
  tbl_df()
saveRDS(all_tweets,"all_tweets.RDS")

library(forcats)
library(scales)
all_tweets %>%
  mutate(source = fct_lump(source, 5)) %>%
  count(month = round_date(created_at, "month"), source) %>%
  complete(month, source, fill = list(n = 0)) %>%
  mutate(source = reorder(source, -n, sum)) %>%
  group_by(month) %>%
  mutate(percent = n / sum(n),
         maximum = cumsum(percent),
         minimum = lag(maximum, 1, 0)) %>%
  ggplot(aes(month, ymin = minimum, ymax = maximum, fill = source)) +
  geom_ribbon() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Time",
       y = "% of Trump's tweets",
       fill = "Source",
       title = "Source of @realDonaldTrump tweets over time",
       subtitle = "Summarized by month")

trump_tweets <- subset(all_tweets,is_retweet==FALSE)
trump_tweets <- subset(trump_tweets,created_at>='2017-12-31')
trumpt <- as.data.frame(trump_tweets[,c(4,3)])

library(xts)
spxt=readRDS('spxt.RDS')
trumpt$toclose=0.0
trumpt$price=0.0
maxdatetime=time(tail(spxt,1))
trumpt=trumpt[trumpt$created_at<maxdatetime,]
mindatetime=time(head(spxt,1))
trumpt=trumpt[trumpt$created_at>mindatetime,]

for (i in 1:nrow(trumpt)){
  trumpt$toclose[i]=(head(spxt[index(spxt)>=trumpt$created_at[i]],1))$toclose
  trumpt$price[i]=(head(spxt[index(spxt)>=trumpt$created_at[i]],1))$price
}
saveRDS(trumpt,'trumpt.RDS')

trumpt$marketimpact=ifelse(trumpt$toclose>=0,"BULL","BEAR")
trumpt$marketimpact <- factor(trumpt$marketimpact)

library(ggplot2)
ggplot(data = trumpt, aes(x = created_at, y = toclose, colour = marketimpact)) + 
  geom_line() + 
  xlab('2018') +
  ylab('Price Chg to Close')

#Text mining and word cloud fundamentals in R
bull <- subset(trumpt, marketimpact == "BULL")
bear <- subset(trumpt, marketimpact == "BEAR")

library(tm)
docs <- Corpus(VectorSource(bull$text))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

docs <- Corpus(VectorSource(bear$text))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Clean Text
#install.packages("qdapRegex")
#install.packages("twitteR")
library(qdapRegex)
library(twitteR)
trump_corpus <- VCorpus(VectorSource(trumpt$text))

cleantrump <- function(t){
  t=gsub("&amp", "", t)
  t=gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", t)
  t=gsub("@\\w+", "", t)
  t=rm_twitter_url(t)#gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*) ","",t)
  t=rm_url(t)#gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*) ","",t)
  t=rm_non_ascii(t)#gsub("[^[:alnum:][:blank:]?&/\\-]", "", t)
  t=tolower(t)
  t=removeWords(t,stopwords())
  t=gsub("^\\s+|\\s+$", "", t)
  t=rm_hash(rm_tag(t))
  t=rm_number(t)
  t=removePunctuation(t)
  #  t=rm_white(t)
  
  t=rm_non_words(t)
  return(t)
}

#install.packages("wordcloud")
library(wordcloud)
wordcloud(cleantrump(bull$text), min.freq = 50, random.order = FALSE)
wordcloud(cleantrump(bear$text), min.freq = 50, random.order = FALSE)

trump_corpus_clean <- tm_map(trump_corpus,content_transformer(cleantrump))

as.character(trump_corpus[1:3])
as.character(trump_corpus_clean[1:3])


wordcloud(trump_corpus_clean, min.freq = 50, random.order = FALSE)

#Splitting Text into Words
trump_corpus_dtm <- DocumentTermMatrix(trump_corpus_clean)
dim(trump_corpus_dtm)
trump_corpus_dtm$dimnames$Terms

#Only examine frequently used words
trump_freq_words=findFreqTerms(trump_corpus_dtm, 10)
trump_corpus_dtm_freq <- trump_corpus_dtm[ , trump_freq_words]
dim(trump_corpus_dtm_freq)
trump_corpus_dtm_freq$dimnames$Terms

#Convert numerical feasures to categorical features
inspect(trump_corpus_dtm_freq[1:10,1:10])

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

trump_dateset <- apply(trump_corpus_dtm_freq, MARGIN = 2,
      convert_counts)

trump_dateset[1:10,1:10]

#Training and Testing Sets
library(caret)
set.seed(4218)
intrain<-createDataPartition(y=trumpt$marketimpact,p=0.7,list=FALSE)

trump_train <- trump_dateset[intrain, ]
trump_test <- trump_dateset[-intrain, ]

trump_train_labels <- trumpt[intrain, ]$marketimpact
trump_test_labels <- trumpt[-intrain, ]$marketimpact

prop.table(table(trump_train_labels))
prop.table(table(trump_test_labels))

#Training the model
library(e1071)
trump_classifier <- naiveBayes(trump_train, trump_train_labels)

#Testing
trump_test_pred <- predict(trump_classifier, trump_test)

library(gmodels)

length(trump_test_pred)

CrossTable(trump_test_pred, trump_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#Trading Simulation
trumpt2=cbind(a <- trumpt[-intrain, ],trump_test_pred)
write.csv(trumpt2,"trumpt2.csv")


trump_test2 <- trumpt[-intrain, ]
trump_test2$predict=trump_test_pred
trump_test2$cumulative_longonly=0
trump_test2$cumulative_strategy=0
trump_test2$cumulative_strategy_2=0

cumulative_longonly=0
cumulative_strategy=0
cumulative_strategy_2=0
for (i in 1:nrow(trump_test2)){
  trump_test2$cumulative_longonly[i]=cumulative_longonly+100*trump_test2$toclose[i]
  if (trump_test_pred[i]=='BULL'){
    trump_test2$cumulative_strategy[i]=cumulative_strategy+100*trump_test2$toclose[i]
  } else {
    trump_test2$cumulative_strategy[i]=cumulative_strategy-100*trump_test2$toclose[i]
  }
  
  cumulative_longonly=trump_test2$cumulative_longonly[i]
  cumulative_strategy=trump_test2$cumulative_strategy[i]
}
bull_count=0
bear_count=0
for (i in 2:nrow(trump_test2)){
  if (trump_test_pred[i]=='BULL' && trump_test_pred[i-1]=='BULL'){
    trump_test2$cumulative_strategy_2[i]=cumulative_strategy_2+100*trump_test2$toclose[i]
    bull_count=bull_count+1
  }
  else if (trump_test_pred[i]=='BEAR' && trump_test_pred[i-1]=='BEAR'){
    trump_test2$cumulative_strategy_2[i]=cumulative_strategy_2-100*trump_test2$toclose[i]
    bear_count=bear_count+1
  }
  else if (trump_test_pred[i]=='BULL' && trump_test_pred[i-1]=='BEAR'){
    trump_test2$cumulative_strategy_2[i]=cumulative_strategy_2+bull_count*100*trump_test2$toclose[i]
    bull_count=0
  }
  else if (trump_test_pred[i]=='BEAR' && trump_test_pred[i-1]=='BULL'){
    trump_test2$cumulative_strategy_2[i]=cumulative_strategy_2-bear_count*100*trump_test2$toclose[i]
    bear_count=0
  }
  cumulative_strategy_2=trump_test2$cumulative_strategy_2[i]
}

ggplot(trump_test2, aes(created_at)) + 
  geom_line(aes(y = cumulative_strategy, color='blue')) +
  geom_line(aes(y = cumulative_strategy_2, color='green')) +
  geom_line(aes(y = cumulative_longonly, color='red')) + 

  labs(x = "Time",
       y = "Cumulative PnL $",
       title = "Trading Simulation",
       color = "")+
  scale_colour_manual(labels = c("Strategy","Strategy_2", "Long Only"), values = c("blue", "green", "red"))
