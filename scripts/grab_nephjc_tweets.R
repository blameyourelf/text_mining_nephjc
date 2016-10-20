# code and inspiration from:
# http://rpubs.com/Antreas93/219057?utm_content=bufferef7cc&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

#install.packages("tm") ; install.packages("httpuv")
#install.packages('base64enc') ;install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("RColorBrewer") ; install.packages("tm") 
#install.packages("stringr")

library(wordcloud)
library(RColorBrewer)
library(tm)
library(stringr)
library(twitteR)
library(httr)
library(devtools)
library(base64enc)

#These keys are required in order to grab tweets 
# See https://twittercommunity.com/t/how-do-i-find-my-consumer-key-and-secret/646 for instructions
consumer_key <- "Your consumer key"
consumer_secret <- "Your consumer secret"
access_token <- "etc"
access_secret <- "etc"

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret = consumer_secret,
                    access_token = access_token,
                    access_secret = access_secret)

text_nephjc <- searchTwitter("#nephjc", n=2000, lang = "en",since = "2016-10-18")
#saveRDS(text_nephjc, "data/161020_nephjc_twit_data.rds")
#text_nephjc <- readRDS("data/161020_nephjc_twit_data.rds")

raw_text_preclean <- sapply(text_nephjc, function(x) x$getText())

clean_tweet <- gsub("&amp", "", raw_text_preclean)
#remove ellipses separating words without spaces (otherwise causes problems with conjoined words when punctuation removed)
clean_tweet <- gsub("...", " ", clean_tweet, fixed=TRUE)
#remove RT
clean_tweet <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)

##extract twitter handles contributors cloud
at_names <- regexpr("@\\w+", clean_tweet)
tweeters <- regmatches(clean_tweet, at_names)

#remove twitter handles, URLs, punctuation and digits
clean_tweet <- gsub("@\\w+", "", clean_tweet)
clean_tweet <- gsub("http\\S+\\s*", "", clean_tweet)
clean_tweet <- gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)

#remove rogue \n new line entries and final cleanup
clean_tweet <- gsub("\n", "", clean_tweet) #remove \n from tweets
clean_tweet <- str_replace_all(clean_tweet,"[^a-zA-Z\\s]", " ")

#create a corpus, and a corpus dictionary for later stemcompletion
dtsc_corpus <- Corpus(VectorSource(clean_tweet))
dtsc_corpus <- tm_map(dtsc_corpus,PlainTextDocument)
dictCorpus <- dtsc_corpus


#generate term document matrix with appropriate stopwords - NB crescents remains- and is still the most popular word
tdm <- TermDocumentMatrix(
  dtsc_corpus,
  control = list(
    stopwords = c("nephjc","amp", "iga", "crescent", "crescenteric", "crescentic", "sbz", "else",
                  stopwords("english")))
)

#convert to matrix and count word frequency
m<-as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)

#stemcompletion (combine words with similar root)
mynames<- names(word_freqs)
mynames <- stemDocument(mynames, language = "english")
mynames <- stemCompletion(mynames, dictionary=dictCorpus, type = "prevalent")
names(word_freqs) <- mynames

#create a dataframe, and then group_by word to bring together stemmed words
dm <- data.frame(word = names(word_freqs), freq = word_freqs)
library(dplyr)
dm2 <- dm %>% group_by(word) %>% summarise(freq = sum(freq))
head(dm2)
dm2 <- dm2[-1,]
head(dm2)

#convert crescent (generated from crescents in the stemming) to crescents, and split iganephropathy for presentation
dm2$word <- as.character(dm2$word)
crescent_loc <- dm2$word == "crescent"
dm2$word[crescent_loc] <- "crescents"
dm2$word[dm2$word == "iganephropathy"] <- "iga nephropathy"

#generate the word cloud and save to pdf
pdf("figures/nephjc_iga_crescent_wc.pdf")
wordcloud(dm2$word, dm2$freq, random.order = FALSE,scale=c(4,0.75), 
          colors = brewer.pal(8, "Dark2"), use.r.layout = FALSE)
dev.off()


#follow similar steps to create wordcloud of twitter handles
tweeters_corpus <- Corpus(VectorSource(tweeters))

tdm_tweeter <- TermDocumentMatrix(
  tweeters_corpus
)

tweeter_m<-as.matrix(tdm_tweeter)
word_freqs_tweeters <- sort(rowSums(tweeter_m), decreasing = TRUE)

dm_tw <- data.frame(word = names(word_freqs_tweeters), freq = word_freqs_tweeters)

pdf("figures/nephjc_iga_crescent_contributors.pdf")
wordcloud(dm_tw$word, dm_tw$freq, random.order = FALSE, scale=c(3,0.75), 
          colors = brewer.pal(8, "Dark2"), min.freq = 1, rot.per = 0.1)
dev.off()
