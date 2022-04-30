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
library(data.table)
library(streamR)

library(dplyr)


library(streamR)
library(ROAuth)
# parse tweets from the stream API ####
# put all your tweet json files in a folder inside of your working directory
# use the name of the folder with the path as first argument replace "your_folder_name"
# put all your tweet json files in a folder inside of your working directory
# use the name of the folder with the path as first argument replace "your_folder_name"
file_names <- list.files("Twitter", full.names = T)
length(file_names)
require(streamR)
# save the dfs in a list - prepare the list
tweets_list <- list()
# loop through the files
for (i in 1:length(file_names)) {
  tweets_list[[i]]  <- parseTweets(file_names[i], simplify = FALSE)
  message(paste(i,"...",sep=""))
}


combine_all_tweets <- list()
for(i in 1:length(tweets_list)){
  combine_all_tweets <- rbind(tweets_list[[i]], combine_all_tweets)
  message(i)
}


library(data.table)
head(tweets_list)
length(tweets_list)
typeof(tweets_list)

head(combine_all_tweets)
length(combine_all_tweets)
nrow(combine_all_tweets)
typeof(combine_all_tweets)




#strptime() to change the $created_at from charater to POSIXlt time for ts_plot() to run
?strptime

time_tweets <- list()
time_tweets <- parseTweets("my_tweets_2020_12_11_10_08.json", simplify = FALSE, verbose = TRUE, legacy = FALSE)
time_tweets$created_at <- strptime(my_tweets_2020_12_11_10_08.json$created_at, format = "%Y-%m-%d %H:%M:%S",tz="UTC")

time_tweets$created_at
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
strptime("Tue, 23 Mar 2010 14:36:38 -0400", "%a, %d %b %Y %H:%M:%S %z")
time_tweets$created_at <- as.POSIXct(strptime(time_tweets$created_at, "%a %b %d %H:%M:%S %z %Y"))

library(rtweet)

ts_plot(test_tweets, "15 minutes")







# Tweet time ts_plot()
# we have to use group_by with the df. As second argument we add the name of the column for the groups (our lines in the plot)
ts_plot(combine_all_tweets , "15 minutes") +
  
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%m/%d %H:%M", tz = "Europe/Berlin")) +
  # UTC
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), axis.text.x=element_text(angle=90, hjust = 1, vjust=0.5)) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets with the #Taiwan and #Korea",
    subtitle = "Tweet counts aggregated using 15-minute intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")




#Example
# assumption here is you collected tweets and have also a df with the information for all unique users (users_df)
# get most active users
users_df <- data.frame(date_time = combine_all_tweets[1]$created_at,
                             username = combine_all_tweets[1]$screen_name,
                             tweet_text = combine_all_tweets[1]$text,
                             coords = combine_all_tweets[1]$coordinates)

most_active <- as.data.frame(table(combine_all_tweets$user_id), stringsAsFactors = F)
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







#language users tweeting:
table_df <- function(data_vector) {
  df <- as.data.frame(table(data_vector), stringsAsFactors=F)
  df <- df[order(df$Freq, decreasing = T), ]
  return(df)
}


lang_table <- table_df(combine_all_tweets$lang)
lang_table <- lang_table[1:20, ]
colnames(lang_table)
ggplot() + geom_bar(data=lang_table, aes(x = data_vector, y= Freq), stat="identity")
ggplot() + geom_bar(data=lang_table, aes(x = reorder(data_vector, Freq), y= Freq), stat="identity") +
  theme(axis.text.x=element_text(angle=90,size=14))  # rotate labels 90°








#Location mapping
#Resource URL:
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/map-tweet-locations-over-time-r/
# create new df with just the tweet texts & usernames
tweet_data <- data.frame(date_time = tweets_list$created_at,
                         username = tweets_list$screen_name,
                         tweet_text = tweets_list$text,
                         location = tweets_list$location)
tweets.df <-twListToDF(tweets)

####Location Mapping GoogleMaps
library(RgoogleMaps)
library(maps)
library(data.table)

# identify location
# we only can get the location if a place is mentioned
check_location <- combine_all_tweets[!tweets_1$location == "", ]
check_location$location
length(check_location$location)
# initiate empty list
location_list <- list()
#getGeoCode(check_location$location[])
for( i in 50622:nrow(check_location)) {
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
#sum: 78485; now:22413
length(location_list)
dev.off


locations_df <- rbindlist(location_list)
locations_df$country <- map.where(database="world", locations_df$lon, locations_df$lat)
table(locations_df$country)
head(locations_df$country)
#create a map without the numbers at the margin using theme_map()
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()
#now show the world map with the points of tweeters locations
world_basemap +
  geom_point(data = locations_df, aes(x = lon, y = lat),
             colour = 'brown', alpha = 0.5, shape = ".") +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  geom_point(aes(x = -87.3, y = 39.4),
             colour = 'black', alpha = 1, shape = 17) +
  labs(title = "People Who Tweeted the Names of Executors")
#save the file by choosing the resolution using ggsave
ggsave(filename="PeopleTweeted.jpg",
       width = 100, height = 50, 
       units = "cm", # other options are "in", "cm", "mm" 
       dpi = 700
)




#Gender
library(gender)

user_all_tweets <- users_data(combine_all_tweets)$name

str_extract(user_all_tweets, "\\w+")[1:10]
user_all_tweets <- str_extract(user_all_tweets, "\\w+")
user_all_tweets <- str_replace_all(user_all_tweets,"[^[:graph:]]", "") 
user_all_tweets <- iconv(user_all_tweets, 'UTF-8', 'ASCII')
user_all_tweets <- str_replace_all(user_all_tweets, "\\d", "") 
user_all_tweets
#now get the proportion of the genders
usergender_combine_tweets <- gender(names= user_all_tweets, method="ssa")
mean(usergender_combine_tweets$proportion_male) #result: 0.3860072
mean(usergender_combine_tweets$proportion_female) #result:0.6139928






#Sentiment
# Source of functions: https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/R/sentiment.R
# general code: https://sites.google.com/site/miningtwitter/questions/sentiment/analysis
# Optimized by: Adrian Rauchfleisch
# function score.sentiment
# you can also use this with Chinese text - you can find the example in another script
library(scales)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)

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
all_tweets_txt <- combine_all_tweets$text
all_tweets_txt <- str_replace_all(all_tweets_txt,"[^[:graph:]]", " ") 


# join texts
deathpost <- all_tweets_txt

# apply function score.sentiment
scores <- score_sentiment(deathpost, pos, neg, .progress='text', chinese = FALSE)
scores

# add variables to data frame
scores$deathpost <- factor(rep(c("Brandon Bernard", "Alfred Bourgeios", "Lisa Montgomery", "Dustin John Higgs"), nd))
# this is an arbitrary choice - it can also be higher and it depends on the length of the text
# 2 and -2 seem to be reasonable for a tweet text

#######I want to get a list of people who has the score number >2 and the ones <2 to do the mapping
scores$very.pos <- as.numeric(scores$score >= 2)
scores$very.neg <- as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos <- sum(scores$very.pos) #result=6920
numneg <- sum(scores$very.neg) #result=31234




###Compare the difference (mapping and gender) between negative and positive tweets 
negative_tweets <- combine_all_tweets[combine_all_tweets$sentimentneg == 1, ]
positive_tweets <- combine_all_tweets[combine_all_tweets$sentiment    == 1, ]

dim(positive_tweets)
length(positive_tweets)
dim(negative_tweets)
length(negative_tweets)



#######now do the mapping again
##Location Mapping GoogleMaps
library(RgoogleMaps)
library(maps)
library(data.table)



# identify location
# we only can get the location if a place is mentioned
check_location_negative <- negative_tweets[!negative_tweets$location == "", ]
check_location_negative$location
check_location_positive <- positive_tweets[!positive_tweets$location =="", ]
check_location_positive$location

# initiate empty list
location_list_negative <- list()
location_list_positive <- list()

####getGeoCode(check_location$location[])
#negative location list
for( i in 21977:nrow(check_location_negative)) {
  temp_result <- try(getGeoCode(check_location_negative$location[i]))
  if(class(temp_result) == "try-error") {
    next
  }
  location_list_negative[[i]] <-   data.frame(user_id = check_location_negative$user_id[i],
                                     location = check_location_negative$location[i],
                                     lat = temp_result[1],
                                     lon = temp_result[2])
  if(i %% 10 == 0) {
    message(i)
  }
}
length(location_list_negative) #sum: 31234; now: 31234


#positive location list
for( i in 205:nrow(check_location_positive)) {
  temp_result <- try(getGeoCode(check_location_positive$location[i]))
  if(class(temp_result) == "try-error") {
    next
  }
  location_list_positive[[i]] <-   data.frame(user_id = check_location_positive$user_id[i],
                                              location = check_location_positive$location[i],
                                              lat = temp_result[1],
                                              lon = temp_result[2])
  if(i %% 10 == 0) {
    message(i)
  }
}
length(location_list_positive) #sum: 6920; now: 6920

dev.off


locations_df_negative <- rbindlist(location_list_negative)
locations_df_negative$country <- map.where(database="world", locations_df_negative$lon, locations_df_negative$lat)
table(locations_df_negative$country)
head(locations_df_negative$country)


locations_df_positive <- rbindlist(location_list_positive)
locations_df_positive$country <- map.where(database="world", locations_df_positive$lon, locations_df_positive$lat)
table(locations_df_positive$country)
head(locations_df_positive$country)
#create a map without the numbers at the margin using theme_map()
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()
#now show the world map with the points of tweeters locations
world_basemap +
  
  geom_point(data = locations_df_negative, aes(x = lon, y = lat),
             colour = 'brown', alpha = 1, shape = ".") +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  geom_point(data = locations_df_positive, aes(x = lon, y = lat),
             colour = 'blue', alpha = 1, shape = ".") +
  geom_point(aes(x = -87.3, y = 39.4),
             colour = 'black', alpha = 1, shape = 17) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "People Tweeted the Names of Executors", subtitle = "Brown: Very Negative (31234 tweets)\nBlue:Very Positive (6920 tweets)")
#save the file by choosing the resolution using ggsave



ggsave(filename="NegativeAndPositiveTweets.jpg",
       width = 100, height = 50, 
       units = "cm", # other options are "in", "cm", "mm" 
       dpi = 700
)










#####Now do the gender analysis again for comparing the pos and neg sentiment lists
library(gender)

#for positive tweets
user_positive_tweets <- users_data(positive_tweets)$name
user_negative_tweets <- users_data(negative_tweets)$name

str_extract(user_positive_tweets, "\\w+")[1:10]
user_positive_tweets <- str_extract(user_positive_tweets, "\\w+")
user_positive_tweets <- str_replace_all(user_positive_tweets,"[^[:graph:]]", "") 
user_positive_tweets <- iconv(user_positive_tweets, 'UTF-8', 'ASCII')
user_positive_tweets <- str_replace_all(user_positive_tweets, "\\d", "") 
user_positive_tweets
#now get the proportion of the genders
usergender_positive_tweets <- gender(names= user_positive_tweets, method="ssa")
mean(usergender_positive_tweets$proportion_male) #result=0.3925021
mean(usergender_positive_tweets$proportion_female) #result=0.6074979


#for negative tweets
user_negative_tweets <- users_data(negative_tweets)$name

str_extract(user_negative_tweets, "\\w+")[1:10]
user_negative_tweets <- str_extract(user_negative_tweets, "\\w+")
user_negative_tweets <- str_replace_all(user_negative_tweets,"[^[:graph:]]", "") 
user_negative_tweets <- iconv(user_negative_tweets, 'UTF-8', 'ASCII')
user_negative_tweets <- str_replace_all(user_negative_tweets, "\\d", "") 
user_negative_tweets
#now get the proportion of the genders
user_negative_tweets <- gender(names= user_negative_tweets, method="ssa")
mean(user_negative_tweets$proportion_male) #result: 0.4276212
mean(user_negative_tweets$proportion_female) #result: 0.5723788




##Check the valadity of the Sentiment Test
# install oolong
devtools::install_github("chainsawriot/oolong")
library(oolong)
# oolong takes a random sample of 1% - thus first use a random sample of 2000 if you want 20
scores_sample <- scores[!duplicated(scores$text), ]
scores_sample <- scores_sample[scores_sample(1:nrow(scores_sample), 2000), ]
oolong_test <- create_oolong(input_corpus = scores_sample$text, construct = "positive", )
# do the test
oolong_test$do_gold_standard_test()
oolong_test$lock(force = TRUE)
# we can now get a quanteda object
scores_coded <- oolong_test$turn_gold()

library(sotu)
library(quanteda)
# we need to match the random sample with our original data and then add the automatic sentiment to the quanteda object
scores_coded$sentiment <- scores_sample[match(texts(scores_coded), scores_sample$text), ]$score  

# now we can check the correlation - report this result
# if you get a correlation higher than 0.4 it's already pretty good
cor.test(scores_coded$sentiment, scores_coded$answer)