#Sentiment Analysis

pacman::p_load(here, tidyverse, readxl, writexl, tm, wordcloud, syuzhet, kableExtra, stringr, topicmodels, ggrepel, tidytext)


apple <- read.csv(here("Block2","data", "test.csv"))

str(apple)

corpus <- iconv(apple$tweet)
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('English'))


inspect(corpus[1:5])


removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])


cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:50, 1:20]


pacman::p_load(tidyverse, rtweet, tm, twitteR, wordcloud, class, BiocManager,e1071, RColorBrewer, tidytext, stringr)
appname <- "R_NLP"
ckey <- "ddr9dmRcb6MizKaamOHOqiyVC"
skey <- "KGevcQBTTBkmdYTkH5tf0ENTXoCPL1EVrLLLVmYl5UCaQbP3G5"
atoken <-"971165270172749830-eWFqVB2masFw5z1GxWQKOpAX404TDjr"
stoken <- "Y8TfcJzrld6Zled5WFiMW9MjTcgL4hTCxjK0A7nwXuJ8H"

bearer <- "AAAAAAAAAAAAAAAAAAAAAGf%2BQAEAAAAAALiCWoEdpYlOaJDWWndrAiTqyyM%3DHRiZHGDaOnVOU4uqR06ghfmeFI6NcbRSG6IhSImbwQaVVW6hF3"
twitter_token <- create_token(
  app = appname,
  consumer_key = ckey,
  consumer_secret = skey,
  access_token = atoken,
  access_secret = stoken)

token <- create_token(app = "R_NLP", consumer_key = ckey, consumer_secret = skey, access_token = atoken, access_secret = stoken)

#Next time you start a session load your token with:
  
rtweet::get_token()


set.seed(101)
tweet1 <- search_tweets(q="ein",
                        n = 50,
                        language = "de",
                        include_rts = FALSE
                        
)

setup_twitter_oauth(ckey,skey,atoken,stoken)


## load data 


tweets <- read_csv(here("block2","data","afd.csv"))

k <- tweets %>% group_by(lang) %>% count() %>% rename(Language = lang, 'Number of Tweets' = n)
k



tweets <- tweets %>% 
  filter(lang=="de")


custom_stop_words <- bind_rows(tibble(word = c("twitter", "tco"), lexicon = c("custom")),
                               tibble(word = stopwords("de"), lexicon = c("stopwords")))
tweets_words <- tweets %>%
  mutate(tweet_number = row_number())%>%
  select(tweet_number, text, created_at)%>%
  as_tibble() %>%
  mutate(text = str_replace_all(text, "[^\x01-\x7F]", ""),
         text = str_replace_all(text, "\\.|[[:digit:]]+", ""),
        # text = str_replace_all(text, "https|amp|t.co", ""),
       # text = gsub("http.*","", text))
       #  text = gsub("https.*","", text) %>% 
        text = str_replace_all(text,"&amp;|&lt;|&gt;", ""),
         text = str_replace_all(text, "@\\w+", ""),
      #   text = gsub("@\\w+", "", text) %>% 
         text = str_replace_all(text, "[[:punct:]]", ""),
         text = str_replace_all(text, "#[a-z,A-Z]*",""), 
         text = str_replace_all(text, "http[[:alnum:]]*", "")) %>% 

      tweets_words <- tweets_words %>%  
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words, by = "word")


tweets_words <- tweets %>%
  mutate(tweet_number = row_number())%>%
  select(tweet_number, text, created_at)%>%
  as_tibble() %>%
  mutate(text = str_replace_all(text, "[^\x01-\x7F]", ""),
       #  text = gsub(text, "(RT|via)((?:\\b\\W*@\\w+)+)", ""),
         text = str_replace_all(text, "\\.|[[:digit:]]+", ""),
         text = str_replace_all(text, "https|amp|t.co", ""),
         text = gsub("http.*","", text),
         text = gsub("https.*","", text),
         text = str_replace_all(text,"&amp;|&lt;|&gt;", ""),
         text = str_replace_all(text, "@\\w+", "")) 

tweets_words <- tweets_words %>% 
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words, by = "word")

#tweet_es <- tweets %>% 
#   filter(lang=="es")





#clean_tweet = gsub("&amp", "",tweets)
clean_tweet <- str_replace_all(tweets$text, "&amp", "")
#clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet <- str_replace_all(clean_tweet, "@\\w+", "")
#clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet <- str_replace_all(clean_tweet, "[[:punct:]]", "")
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
clean_tweet <- str_replace_all(clean_tweet, "http[[:alnum:]]*", "")
clean_tweet <- str_replace_all(clean_tweet, stop, "")
clean_tweet <- str_trim(clean_tweet) %>% 
  unnest_tokens(word, text)
 
cleanDF <- as.data.frame(clean_tweet)
clean <- removeWords(cleanDF$clean_tweet, words = stopwords(kind = "de"))  

clean <- as.data.frame(clean)
clean_tweet <- as.data.frame(clean)
#get rid of unnecessary spaces
library(qdap)
clean_tweet <- str_replace_all(clean_tweet, stopwords(kind = "german"), "")

clean_tweet <- str_to_lower(clean_tweet) 


 content(clean_tweet[[1]])
termsmatrix
tdm <- TermDocumentMatrix(tweets_words$word)
tdm <- as.matrix(tdm)
tda <- rowSums(tdm)
tda <- sort(tda, decreasing = TRUE)


cleanDF <- as.data.frame(tda)# %>% 
  unnest_tokens(word, text)

corpus <- iconv(tweets_words$word)
corpus <- Corpus(VectorSource(corpus))

corpus <- iconv(tweets_words$word)
corpus <- Corpus(VectorSource(corpus))



tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]


words <- tweets_words %>% count(word, sort=TRUE)


wordcloud(tdm)

set.seed(1234) # for reproducibility 
wordcloud(words = words$word, freq = words$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

library(rebus)


w <- rowSums(tdm)
w <- subset(w, w>30)
barplot(w,
        las = 2,
        col = rainbow(20))




source(here("Block2","SentiWS.R"))

tweets_sentiment <- tweets_words %>%
  left_join(SentiWS_df, by="word") 



tweets_sentiment %>%
  drop_na() %>%
  mutate("created_at" = as.Date(created_at)) %>%
  group_by(created_at) %>%
  count(Polarity) %>%
  ggplot(aes(x=created_at, y=n, group=Polarity, color=Polarity)) +
  geom_line(size=0.6, alpha=0.6)+
  geom_smooth(span=0.2, se=FALSE, size=0.8)+
  scale_colour_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    x = NULL, y = NULL,
    title = "Anzahl positiver & negativer Wörter",
    subtitle = "aggregiert pro Tag",
    caption = "Plot 1"
  )


#Plot2 - Verhältnis positiver zu negativen Wörtern pro Tag
tweets_sentiment %>%
  drop_na() %>%
  mutate("created_at" = as.Date(created_at)) %>%
  group_by(created_at) %>%
  count(Polarity) %>%
  spread(Polarity, n, fill=0) %>%
  mutate(relation = positive/negative) %>%
  ggplot(aes(x=created_at, y=relation, group=1)) +
  geom_line(size=1, color="#004C99")+
  theme_minimal() +
  labs(
    x = NULL, y = NULL,
    title = "Verhältnis positiver zu negativen Wörtern",
    subtitle = "aggregiert pro Tag",
    caption = "Plot 2")

tweets_sentiment %>%
     drop_na() %>%
     mutate("created_at" = as.Date(created_at)) %>%
     group_by(created_at) %>%
     count(Polarity) %>%
     ggplot(aes(x=created_at, y=n, group=Polarity, color=Polarity)) +
     geom_line(size=0.6, alpha=0.6)+
     geom_smooth(span=0.2, se=FALSE, size=0.8)+
     scale_colour_brewer(palette = "Set1") +
     theme_minimal() +
     labs(
         x = NULL, y = NULL,
         title = "Anzahl positiver & negativer Wörter",
         subtitle = "aggregiert pro Tag",
         caption = "Plot 1"
       )



tweets_sentiment2 <- tweets_sentiment %>% 
  drop_na()

library(tidytext)
nrc <- get_sentiments("nrc")
