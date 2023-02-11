# import libraries

library(tidyverse)
library(SentimentAnalysis)
library(dplyr)
library(parallel)
library(numbers)

# load data

setwd("/Users/ramzi.chariag/Documents/CEU/PhD/ML Prediction")

reviews <- read.csv("./assignment_2/data/reviews.csv")
listings <- read.csv("./assignment_2/data/listings.csv")

# generate sentiment scores and word counts

## pre-processing text:
preprocess_text = function(x)
  {
    # convert to lower case
    x = tolower(x)
    # remove \n
    x = gsub("\\n", "", x)
    # remove tags
    x = gsub("<.*?>", "",x)
    # remove everything that is not alphanumeric, apostrophe or exclamation
    x = gsub("[^[:alnum:][:space:]'!]", "", x)
    # remove at
    x = gsub("@\\w+", "", x)
    # remove numbers
    x = gsub("[[:digit:]]", "", x)
    # remove links http
    x = gsub("http\\w+", "", x)
    # remove blank spaces at the beginning
    x = gsub("^ ", "", x)
    # remove blank spaces at the end
    x = gsub(" $", "", x)
    
    return(x)
}

rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}

reviews <- reviews %>% mutate(comments = preprocess_text(comments),
                              comments = rm_words(comments, tm::stopwords("en"))
                              #word_count = analyzeSentiment(comments)$WordCount,
                              #sentiment = analyzeSentiment(comments)$SentimentQDAP
)

# sentiment analysis in batches
# initialize variables 

size <- nrow(reviews)
lower <- 1     # lower bound of the batch
interval <- min(primeFactors(size))  # numbers of reviews per batch
upper <- lower+interval-1   #upper bound of the batch
iterations <- size / interval   #number of iterations

# reviews[1,7] # 1 is the row and 7 is the comment column

sentiment<- analyzeSentiment(reviews$comments[lower:upper])$SentimentQDAP

for (i in 2:iterations) {
  
  lower <-lower+interval
  upper<-upper+interval
  batch <- reviews$comments[lower:upper]
  
  sentiment <- c(sentiment,analyzeSentiment(batch)$SentimentQDAP) 
  
}

# merge reviews with sentiment
reviews_sentiment <- cbind(reviews,sentiment)
write.csv(reviews_sentiment,"./assignment_2/data/reviews_sentiment.csv")

# number of reviews for each listing
review_frequency <- unique(group_by(reviews_sentiment, listing_id)%>%
                             mutate(num_comm = n()) %>%
                             summarise(num_comm)
)

# merge with number of reviews per listing
listings_freq <- listings %>% left_join(review_frequency, 
                                        by=c('id'='listing_id'))

# only keep the sentiment score and the listing id
keeps <- c("listing_id","sentiment")
reviews_sentiment <- reviews_sentiment[keeps]

# replace NA sentiment with zero, and average over listing id
reviews_sentiment_grouped <- reviews_sentiment %>% 
  mutate(sentiment = replace_na(sentiment,0)) %>%
  group_by(listing_id) %>%
  summarise(sentiment = mean(sentiment))


# merge with average sentiment per listing
listings_merged <- listings %>% left_join(reviews_sentiment_grouped, 
                                          by=c('id'='listing_id'))


# drop intermediate objects
rm(reviews_sentiment, review_frequency,
   reviews_sentiment_grouped, listings_freq, keeps)


write.csv(listings_merged,"./assignment_2/data/listings_sentiment.csv")
