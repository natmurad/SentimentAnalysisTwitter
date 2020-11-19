############################################
#title:  "Sentiment Analysis with Tweets"  #
#author: "Natália Faraj Murad"             #
#email:  "nataliafmurad@gmail.com"         #
############################################

#Setting the directory
#setwd("YOUR DIRETORY")

#Loading packages
#Access Twitter Data
library(twitteR)
library(httr)
library(rtweet)
#Text Mining
library(SnowballC)
library(tm)
library(stringr)
library(plyr)
library(tidyverse)
library(textdata)
library(DT)
library(Rstem)
#Graphic Visualization
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library("lattice")
#Sentiment Analysis
library(sentiment)
#Lexical dictionaries
library(lexiconPT) #Portuguese
library(tidytext)  #English
#ls("package:lexiconPT")


#Setting Twitter authentication keys
key         <- "your key"
secret      <- "your secret"
token       <- "your token"
tokensecret <- "your token secret"

#Use direct connection
twitteR::setup_twitter_oauth(key, secret, token, tokensecret)

twitter_tokens <- rtweet::create_token(app            = "yourapp-app",
                                       consumer_key    = "yourkey", 
                                       consumer_secret = "yoursecret",
                                       access_token    = "yourtoken",
                                       access_secret   = "youraccess")

#Collecting tweets
tweets <- search_tweets(q = "vaccine", n = 10000, lang = "en", include_rts = FALSE)
#head(tweetpt$text)

#Cleaning the tweets
tweets$text <- tweets$text            %>%
  str_to_lower()                      %>%   #lowercase
  str_replace_all("\n", " ")          %>%   #Remove \n
  str_replace_all("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "")                                              %>%   #Remove urls
  str_replace_all("[[:digit:]]", " ") %>%   #Remove digits
  str_replace_all(" *-+ *", " ")      %>%   #Remove hyphens
  str_replace_all("@\\w+", " ")       %>%   #Remove @
  str_replace_all("[[:punct:]]", " ") %>%   #Replace punct by space
  str_replace_all("[ \t]{2,}", " ")   %>%   #Remove spaces
  str_replace_all("^\\s+|\\s+$", " ") %>%   #Remove spaces
  str_replace_all("\\\\", "")         %>%   #Remove backlash
  str_replace_all("\\/", "")          %>%   #Remove backlash
  str_replace_all("\"", "")           %>%   #Remove quotes
  removeNumbers()                     %>%   #Remove numbers
  trimws()    



#Removing stop words.
tweets$text <- tweets$text %>%
  removeWords(words = c("like", "ass", "will", "elsewhere", "now", "via",
                        "shit", "minimal", "maximum", "dont", "re", "said",
                        "hey", "also", "can", "boys", "ned", "seen", "two",
                        "just", "going", "ones", "never", "things", "view",
                        "doesn", "say", "yeh", "let", "tell", "told", "don",
                        "found", "perhaps", "use", "lot", "lots", "many",
                        "much", "hear", "coming", "one", "yes", "hasn", "haven",
                        "wasn", "please", "across", "seeing", "buy",
                        "hyperlinks", "days", "didn", "t", "way", "come",
                        "getting", "non", "full", "even", "group", "already",
                        "except", "lol", "idk", "rlly", "tho", "base", "ain",
                        "though", "wouldn", "along", "indeed", "past", "dont+",
                        "dont", "doent", "wouldnt", "ive", "altough", "ago",
                        "btw", "whether", "went", "pls", "else", "etc", "got",
                        "thing", "keep", "ever", "well", "still", "yet", "good",
                        "bad", "bitch", "damn", "ve", "m", "ill", "s", "y",
                        "really", "d", "im", "youre", "w", "arent", "get",
                        "u", "get", "tf", "wtf", "without", "anyone", "says",
                        "bitches", "niggas",
                        stopwords(kind = "en")))

tweets      <- select(tweets, c("user_id", "text"))

#Separate one word by row
tweetwords  <- tweets    %>%
               unnest_tokens(output = "words", input = text)
tweetwords <- tweetwords %>% filter(str_length(tweetwords$words)>1)

#Make the intersection between the words of our dataset and the dictionary
tweetsent <- inner_join(tweetwords,
                        get_sentiments("afinn")[, c("word", "value")],
                        by = c("words" = "word"))

#Polarity for each tweet
tpol <- tweetsent         %>%
        group_by(user_id) %>%
        summarise(soma = sum(value),
        n = n(),
        sentiment = soma/n)

#dev.off()
#Empirical kernel density of the sentiment score
ggplot(tpol, aes(x = sentiment))              +
  geom_density(fill = "orange", alpha = 0.25) +
  geom_rug()                                  +
  labs(x = "Polarity", y = "Density")

#Relative accumulated frequency
ggplot(tpol, aes(x = sentiment)) +
       stat_ecdf()               +
       geom_rug()                +
       labs(x = "Polarity", y = "Frequency")

#Filter words with not null polarity
tweetsentnotnul <- tweetsent                        %>%
                   count(words, value, sort = TRUE) %>%
                   filter(value != 0)
tweet_cloud     <- tweetsentnotnul                              %>%
                   spread(key = "value", value = "n", fill = 0) %>%
                   rename("negative" = "-1", "positive" = "1")
tpol <- as.data.frame(tweet_cloud[, c("negative", "positive")])
rownames(tpol) <- tweet_cloud$words

#Plot the wordcloud
comparison.cloud(tpol,
                 colors = c("black", "gray"),
                 max.words = min(nrow(tpol), 200))

#Other kind of wordcloud
tweets1 <- as.matrix(tweets$text)

# Gerando uma nuvem palavras
pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweets1, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

#Barplot for more frequent words
n_words <- 5
#tweetsentnotnul <- tweetsentnotnul %>% filter(words != "fake")

tweet_bars <- tweetsentnotnul %>%
  mutate(score = value * n)   %>%
  group_by(value)             %>%
  top_n(n, n = n_words)       %>%
  ungroup()

ggplot(data = tweet_bars, mapping = aes(x = reorder(words, score),
                                        y = score, fill = score))                             +
       geom_col(color = "black")                             +
       scale_fill_distiller(palette = "RdBu", direction = 1) +
       coord_flip()                                          +
       theme_light()                                         +
       theme(legend.position = c(0.95, 0.5),
             legend.justification = c(1, 0.5))               +
       labs(y = "Frequency", x = "Term", fill = "Frequency")

#Nayve Bayes Classifier
#Classifying emotion
class_emo = classify_emotion(tweets, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]

#Replacing NA by "Neutral"
emotion[is.na(emotion)] = "Neutral"

#Classifying polarity
class_pol = classify_polarity(tweets, algorithm = "bayes")
polarity = class_pol[,4]

#Result Dataframe
sent_df = data.frame(text = tweets, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)

#Ordering
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels = names(sort(table(emotion), 
                                                                decreasing=TRUE))))

#Plotting emotions found
ggplot(sent_df, aes(x = emotion))                   +
       geom_bar(aes(y = ..count.., fill = emotion)) +
       scale_fill_brewer(palette = "Dark2")         +
       labs(x = "Categories", y = "Number of Tweets") 

#Plotting the polarity
ggplot(sent_df, aes(x=polarity))                 +
       geom_bar(aes(y=..count.., fill=polarity)) +
       scale_fill_brewer(palette="RdGy")         +
       labs(x = "Sentiment Categories", y = "Number of Tweets")

tweetlist <- iconv(tweets$text, to = "utf-8", sub="")
tweetcorpus <- Corpus(VectorSource(tweetlist))



termo_por_documento = as.matrix(TermDocumentMatrix(tweetcorpus), control = list(stopwords = c(stopwords("english"))))

tweettdm <- TermDocumentMatrix(tweetcorpus)


# Removendo termos esparsos (não utilizados frequentemente)
tweet2tdm <- removeSparseTerms(tweettdm, sparse = 0.9)

# Criando escala nos dados
tweet2tdmscale <- scale(tweet2tdm)

# Distance Matrix
tweetdist <- dist(tweet2tdmscale, method = "euclidean")

# Preparando o dendograma
tweetfit <- hclust(tweetdist)

# Criando o dendograma (verificando como as palvras se agrupam)
plot(tweetfit)

#Creating function to compare the words with a list
sentimento.score = function(sentences, pos.words, neg.words,
                            .progress = 'none'){
  #Create an array with the scores
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words){
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x){
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

#Read lists with the positive and negative words
pos = readLines("palavras_positivas.txt")
neg = readLines("palavras_negativas.txt")

#Getting tweets by country
catweets = searchTwitter("ca", n = 1000, lang = "en")
usatweets = searchTwitter("usa", n = 1000, lang = "en")

#Select the text and store in a vector
catxt = sapply(catweets, function(x) x$getText())
usatxt = sapply(usatweets, function(x) x$getText())
countryTweet = c(length(catxt), length(usatxt))
countries = c(catxt, usatxt)

#Calculate the sentiment score
scores = sentimento.score(countries, pos, neg, .progress = 'text')

#Score by country
scores$countries = factor(rep(c("ca", "usa"), countryTweet))
scores$muito.pos = as.numeric(scores$score >= 1)
scores$muito.neg = as.numeric(scores$score <= -1)

#Total
numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

#Global score
global_score = round( 100 * numpos / (numpos + numneg) )
boxplot(score ~ countries, data = scores)

#Plot the histogram
histogram(data = scores, ~score|countries, main = "Sentiment Analysis", xlab = "", sub = "Score")


