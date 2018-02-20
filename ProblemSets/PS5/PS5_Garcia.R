# Rvest Data scraping Example
library(rvest)
url<- "https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table"

# Extract html table using rvest
# Get xpath file from inspecting website 

Olympics_count <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
Olympics_count <- Olympics_count[[1]]

data.frame(Olympics_count)
str(Olympics_count)

write.csv(Olympics_count, "olympics_count.csv")

df<- read.csv("Olympics_count.csv")
df

# API Data Scraping Example 
library(tm)
library(wordcloud)
library(twitteR)

# Set up twitter authentication
consumerKey <- "tCBNtBaQRepeNH0JgGhtol7Oq"
consumerSecret <- "CaIxE6azNxVfAPumrLSIkOnHAtl3zWN3iuTW28D7m3yPtMPfGz"
accessToken <- "814537699-7LdOe5XsEdszCeW0aLRdIVN4PeYZskSyni0fdpwy"
accessTokenSecret <- "4IQpDjUSpMIkM5O3unxQBHLrhv1Gohnv8bT0DqIHXpM2t"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# Gather data on gun control
GC_twitter<- searchTwitter("gun+control", n=500, lang = "en", resultType = "recent") # Collect data from twitter gc= gun control

# Extract text from twitter data set
GC_twitter_text<- sapply(GC_twitter, function(x) x$getText())

# Use corpus command to create a corpus out of a vector source
GC_twitter_corpus<- Corpus(VectorSource(GC_twitter_text))

# Clean up text
GC_twitter_clean<- tm_map(GC_twitter_corpus, removePunctuation) # remove punctuation

GC_twitter_clean<- tm_map(GC_twitter_clean, content_transformer(tolower)) # lower case

GC_twitter_clean<- tm_map(GC_twitter_clean, removeWords, stopwords("english"))

GC_twitter_clean<- tm_map(GC_twitter_clean, removeNumbers) # remove numbers

GC_twitter_clean<- tm_map(GC_twitter_clean, stripWhitespace) # remove white empty space

GC_twitter_clean<- tm_map(GC_twitter_clean, removeWords, c("gun", "control"))

# Use wordcloud
wordcloud(GC_twitter_clean, random.order = F, max.words = 40, scale = c(3, 0.5))

