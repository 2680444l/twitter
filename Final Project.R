library(rtweet)
library(scales)
library(ggplot2)
library(stringr)

library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)

library(rjson)
library(jsonlite)
library(tidyr)


# Load the package required to read JSON files.
library("rjson")
# Give the input file name to the function.
result <- fromJSON(file = "my_tweets_2020_12_11_10_08.json")
# Convert JSON file to a data frame.
json_data_frame <- as.data.frame(result)
print(json_data_frame)





#Download the tweets with hashtags of #BrandonBernard, #AlfredBourgeois, #LisaMontgomery, #CoryJohnson, #DustinJohnHiggs 

tweets_1 <- search_tweets("#BrandonBernard", n = 30000, token = twitter_token)
tweets_2 <- search_tweets("#AlfredBourgeois", n = 10000, token = twitter_token)
tweets_3 <- search_tweets("#LisaMontgomery", n = 1000, token = twitter_token)
tweets_4 <- search_tweets("#CoryJohnson", n = 1000, token = twitter_token)
tweets_5 <- search_tweets("#DustinJohnHiggs", n = 1000, token = twitter_token)
#first data collected at 2020/12/11 16:13 (Taiwan Time)


#Example
# assumption here is you collected tweets and have also a df with the information for all unique users (users_df)
# get most active users
most_active <- as.data.frame(table(tweets$user_id), stringsAsFactors = F)
most_active <- most_active[order(most_active$Freq, decreasing = T), ]

# get most followed users
users_df <- users_df[order(users_df$followers_count, decreasing = T), ]

# in this case I select the top 100 for each and add a random sample of 100
# change the numbers for your own cases - if you want 500 for each - change the 100 to 500 in the code below
sample_active <- most_active[1:100, ]
sample_foll <- users_df[1:100, ]

# random sample should cover users that are not yet selected:
users_df_ran <- users_df[!users_df$user_id %in% c(sample_active$Var1, sample_foll$user_id), ]
# get a random sample of 100
sample_ran <- users_df_ran[sample(1:nrow(users_df_ran), 100), ]
# merge the users ids
sample_user_ids <- c(sample_active$Var1, sample_foll$user_id, sample_ran$user_id)

# check if all worked, should have 0 duplicated
sum(duplicated(sample_user_ids))









head(tweets_1)
tweets_1_1 <- tweets_1[order(tweets_1$created_at, decreasing = T), ]
unique_user <- tweets_1[!duplicated(all_tweets[ ,"user_id"], fromLast = TRUE), c("user_id","created_at")] # 


#language users tweeting:
table_df <- function(data_vector) {
  df <- as.data.frame(table(data_vector), stringsAsFactors=F)
  df <- df[order(df$Freq, decreasing = T), ]
  return(df)
}

lang_table <- table_df(tweets_1_1$lang)
lang_table <- lang_table[1:20, ]
colnames(lang_table)
ggplot() + geom_bar(data=lang_table, aes(x = data_vector, y= Freq), stat="identity")
ggplot() + geom_bar(data=lang_table, aes(x = reorder(data_vector, Freq), y= Freq), stat="identity") +
  theme(axis.text.x=element_text(angle=90,size=14))  # rotate labels 90°









#Location mapping
#Resource URL:
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/map-tweet-locations-over-time-r/
# create new df with just the tweet texts & usernames
tweet_data <- data.frame(date_time = tweets_1$created_at,
                         username = tweets_1$screen_name,
                         tweet_text = tweets_1$text,
                         coords = tweets_1$coordinates)

tweets.df <-twListToDF(tweets)


# cleanup & and filter to just the time period around the flood


world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80")

world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

world_basemap


world_basemap +
  geom_point(data = tweet_locations, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "People who tweeted for executions")







#Location Mapping GoogleMaps
library(RgoogleMaps)
library(maps)
library(data.table)

# identify location
# we only can get the location if a place is mentioned
check_location <- tweets_1[!tweets_1$location == "", ]
check_location$location

# initiate empty list
location_list <- list()
#getGeoCode(check_location$location[])


for( i in 100:nrow(check_location) ) {
  temp_result <- try(getGeoCode(check_location$location[i]))
  if(class(temp_result) == "try-error") {
    next
  }
  location_list[[i]] <-   data.frame(user_id = check_location$user_id[i],
                                     location = check_location$location[i],
                                     lat = temp_result[1],
                                     lon = temp_result[2])
  if(i %% 10 == 0) {
    message(i)
  }
}


library(jsonlite)
my_tweets_2020_12_11_10_08.json <- jsonlite::stream_in(file("my_tweets_2020_12_11_10_08.json"))


locations_df <- rbindlist(location_list)
locations_df$country <- map.where(database="world", locations_df$lon, locations_df$lat)
table(locations_df$country)


world_basemap +
  geom_point(data = locations_df, aes(x = lon, y = lat),
             colour = 'blue', alpha = .2) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "People Who Tweeted for Executions")







# here is some code with saves in between (every 3600 seconds in this example)
# .json files will be saved in the working directory
# here you can define the end date
# can be far in the future - you can manually stop if you want to stop
# even if RStudio crashes, the files are saved externally in the working directory
# if you want to track more than one keyword - you need to use commas
# "you can think of commas as logical ORs, while spaces are equivalent to
# logical ANDs (e.g. ‘the twitter’ is the AND twitter, and ‘the,twitter’ is the OR twitter)"
# https://developer.twitter.com/en/docs/tweets/filter-realtime/guides/basic-stream-parameters
#
end.date <- as.Date("2019-12-31")
# if you run the next lines of code it will run until it reaches the end date
# you can anytime close RStudio - data up to that point will be saved in the working diretcory
while (Sys.Date() < end.date){
  ## preparing file name so that it mentions time
  current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
  file.name <- paste("my_tweets_", current.time, ".json", sep="")
  ## capture tweets
  filterStream( file=file.name, track="keyword", oauth=my_oauth, timeout=3600)
}












#Gender
library(gender)

user_tweets_1 <- users_data(tweets_1)$name
user_tweets_2 <- users_data(tweets_2)$name
user_tweets_3 <- users_data(tweets_3)$name
user_tweets_4 <- users_data(tweets_4)$name
user_tweets_5 <- users_data(tweets_5)$name


str_extract(user_tweets_1, "\\w+")[1:10]
user_tweets_1 <- str_extract(user_tweets_1, "\\w+")
user_tweets_1 <- str_replace_all(user_tweets_1,"[^[:graph:]]", "") 
user_tweets_1 <- iconv(user_tweets_1, 'UTF-8', 'ASCII')
user_tweets_1 <- str_replace_all(user_tweets_1, "\\d", "") 
user_tweets_1

user_tweets_2 <- str_extract(user_tweets_2, "\\w+")
user_tweets_2 <- str_replace_all(user_tweets_2,"[^[:graph:]]", "") 
user_tweets_2 <- iconv(user_tweets_2, 'UTF-8', 'ASCII')
user_tweets_2 <- str_replace_all(user_tweets_2, "\\d", "") 
user_tweets_2

usergender_tweets_1 <- gender(names= user_tweets_1, method="ssa")
mean(usergender_tweets_1$proportion_male)

usergender_tweets_2 <- gender(names= user_tweets_2, method="ssa")
mean(usergender_tweets_2$proportion_male)

t.test(usergender_tweets_1[,]$proportion_male, usergender_tweets_2$proportion_male)








#Sentiment
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)

# Source of functions: https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/R/sentiment.R
# general code: https://sites.google.com/site/miningtwitter/questions/sentiment/analysis
# Optimized by: Adrian Rauchfleisch
# function score.sentiment
# you can also use this with Chinese text - you can find the example in another script
score_sentiment <- function(sentences, pos.words, neg.words, .progress='none', chinese=FALSE, jie_worker) {
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    if ( !chinese ) {
                      # remove punctuation
                      sentence <- gsub("[[:punct:]]", "", sentence)
                      # remove control characters
                      sentence <- gsub("[[:cntrl:]]", "", sentence)
                      # remove digits?
                      sentence <- gsub('\\d+', '', sentence)
                      
                      # define error handling function when trying tolower
                      tryTolower <- function(x)
                      {
                        # create missing value
                        y <- NA
                        # tryCatch error
                        try_error <- tryCatch(tolower(x), error=function(e) e)
                        # if not an error
                        if (!inherits(try_error, "error"))
                          y <- tolower(x)
                        # result
                        return(y)
                      }
                      # use tryTolower with sapply 
                      # if Chinese, don't use tolower:
                      sentence <- sapply(sentence, tryTolower)
                    }
                    
                    if ( !chinese) {
                      # split sentence into words with str_split (stringr package)
                      word.list <- str_split(sentence, "\\s+")
                      words <- unlist(word.list) 
                    } else {
                      word.list <- segment(sentence, jie_worker)
                      words <- unlist(word.list) 
                    }
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos.words)
                    neg.matches <- match(words, neg.words)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}

# import positive and negative words
pos <- readLines("positive_words.txt")
neg <- readLines("negative_words.txt")

# compare drinks
#wine_tweets <- search_tweets("wine", n=500, lang="en", token = twitter_token)
#beer_tweets <- search_tweets("beer", n=500, lang="en", token = twitter_token)
#cofe_tweets <- search_tweets("coffee", n=500, lang="en", token = twitter_token)
#soda_tweets <- search_tweets("soda", n=500, lang="en", token = twitter_token)

# get text
tweets_1_txt <- tweets_1$text
tweets_2_txt <- tweets_2$text

tweets_1_txt <- str_replace_all(tweets_1_txt,"[^[:graph:]]", " ") 
tweets_2_txt <- str_replace_all(tweets_2_txt,"[^[:graph:]]", " ") 


# how many tweets of each drink
nd <- c(length(tweets_1_txt), length(tweets_2_txt))

# join texts
deathpost <- c(tweets_1_txt, tweets_2_txt) 

# apply function score.sentiment
scores <- score_sentiment(deathpost, pos, neg, .progress='text', chinese = FALSE)

# add variables to data frame
scores$deathpost <- factor(rep(c("wine", "beer", "coffee", "soda"), nd))
# this is an arbitrary choice - it can also be higher and it depends on the length of the text
# 2 and -2 seem to be reasonable for a tweet text
scores$very.pos <- as.numeric(scores$score >= 2)
scores$very.neg <- as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)

# global score
global_score <- round( 100 * numpos / (numpos + numneg) )

# colors
cols <- c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
names(cols) <- c("brandon", "alfred")

# boxplot
ggplot(scores, aes(x=deathpost, y=score, group=deathpost)) +
  geom_boxplot(aes(fill=deathpost)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) +
  ggtitle("Boxplot - Drink's Sentiment Scores")

# barplot of average score
df <- ddply(scores, .(deathpost), summarise, meanscore=mean(score))
df$drink <- reorder(df$deathpost, df$meanscore)

ggplot() +
  geom_bar(data=df, aes(y=meanscore, x=drink, fill=drink),stat="identity") +
  scale_fill_manual(values=cols[order(df$drink)]) +
  ggtitle("Average Sentiment Score")

# barplot of average very positive
drink_pos <- ddply(scores, .(drink), summarise, mean_pos=mean(very.pos))
drink_pos$drink <- reorder(drink_pos$drink, drink_pos$mean_pos)

ggplot() +
  geom_bar(data=drink_pos, aes(y=mean_pos, x=drink, fill=drink),stat="identity") +
  scale_fill_manual(values=cols[order(drink_pos$drink)]) +
  ggtitle("Average Very Positive Sentiment Score")

# barplot of average very negative
drink_neg <- ddply(scores, .(drink), summarise, mean_neg=mean(very.neg))
drink_neg$drink <- reorder(drink_neg$drink, drink_neg$mean_neg)

ggplot() +
  geom_bar(data=drink_neg, aes(y=mean_neg, x=drink, fill=drink),stat="identity") +
  scale_fill_manual(values=cols[order(drink_neg$mean_neg)]) +
  ggtitle("Average Very Negative Sentiment Score")




library(streamR)
library(ROAuth)

# here you need to enter your consumer key and your consumer secret
# you find them here: https://apps.twitter.com/ when you click on your app
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
# here replace the placeholder with your consumer key and secret
consumerKey <- "your_consumer_key"
consumerSecret <- "your_consumer_secret"
accessToken <- "your_access_token"
accessTokenSecret <- "your_access_secret"

# create an oauth object
my_oauth <- createOAuthToken(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# save the verified oauth object. You can use it also in other projects, no need to verify again
save(my_oauth, file = "my_oauth.Rdata")

# you can test the stream, timeout defines how long the connection should stay (120 = 2 minutes)
# file.name defines name of the file it will be saved in the working directory.
# you can test now for 15 minutes a keyword - just replace the placeholder
filterStream(file.name = "test.json", track = "Keyword", timeout = 120, oauth = my_oauth, verbose = TRUE)
# with the next line you import the externally saved file into R
# ignore the number of tweets, it's usually less than it says in the console
test_tweets_df <- parseTweets("test.json", simplify = FALSE, verbose = TRUE)
# did it work?

# here is some code with saves in between (every 3600 seconds in this example)
# .json files will be saved in the working directory
# here you can define the end date
# can be far in the future - you can manually stop if you want to stop
# even if RStudio crashes, the files are saved externally in the working directory
# if you want to track more than one keyword - you need to use commas
# "you can think of commas as logical ORs, while spaces are equivalent to
# logical ANDs (e.g. ‘the twitter’ is the AND twitter, and ‘the,twitter’ is the OR twitter)"
# https://developer.twitter.com/en/docs/tweets/filter-realtime/guides/basic-stream-parameters
#
end.date <- as.Date("2019-12-31")
# if you run the next lines of code it will run until it reaches the end date
# you can anytime close RStudio - data up to that point will be saved in the working diretcory
while (Sys.Date() < end.date){
  ## preparing file name so that it mentions time
  current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
  file.name <- paste("my_tweets_", current.time, ".json", sep="")
  ## capture tweets
  filterStream( file=file.name, track="keyword", oauth=my_oauth, timeout=3600)
}














#Download online verdicts

# from scratch - how to get sentaor data. use scraping
if(!require(rvest)){install.packages("rvest")}
library(rvest)
library(stringr)

senators_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_current_United_States_senators")
verdict <- read_html("https://law.judicial.gov.tw/FJUD/default.aspx")
class(verdict)


# we just get the table directly
html_table(html_node(senators_wiki, "#senators"),fill=T)
senators_table <- senators_wiki %>%
  html_node("#senators") %>%
  # as there is some problem with a column we use fill=T - this will solve the issue
  html_table(fill=T)
table(senators_table$Party)
str(senators_table)
# problem - let's delete column 4
senators_table[ ,4] <- NULL
table(senators_table$Party)

