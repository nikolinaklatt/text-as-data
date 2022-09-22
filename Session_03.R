# working with Chris Bail https://cbail.github.io/textasdata/apis/rmarkdown/Application_Programming_interfaces.html 

# setting up my Twitter API login


# required packages 
library(devtools)
library(usethis)
library(rtweet)
library(httr)
library(askpass)
library(ggplot2)
library(maps)
library(dplyr)
library(MASS)


# authenticate ourselves vis-a-vis Twitter’s API
create_token(
  app=app_name, 
  consumer_key=consumer_key, 
  consumer_secret=consumer_secret,
  access_token = access_token,
  access_secret = access_token_secret)

# getting started with scraping data - extracting 3,000 tweets that use the hashtag #korea.
korea_tweets<-search_tweets("#Korea", n=50, include_rts = FALSE)

head(korea_tweets$text)

# full list of variables we collected via our API call above
names(korea_tweets)

# search_tweets
nk_tweets <- search_tweets("korea",
                           "lang:en", geocode = lookup_coords("usa"), 
                           n = 1000, type="recent", include_rts=FALSE
)

# rtweet also enables one to geocode tweets for users who allow Twitter to track their location:
geocoded <- lat_lng(nk_tweets)

# ploting the geocoded tweets
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
with(geocoded, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

# collecting data about a given user
sanders_tweets <- get_timelines(c("sensanders"), n = 5)
head(sanders_tweets$text)

sanders_twitter_profile <- lookup_users("sensanders")
sanders_twitter_profile$description
sanders_twitter_profile$location
sanders_twitter_profile$followers_count

# get_favorites() function to identify the Tweets Sanders has recently “liked.”
sanders_favorites<-get_favorites("sensanders", n=5)
sanders_favorites$text

# get a list of the people who Sanders follows like this:
sanders_follows<-get_followers("sensanders")

## This produces the user IDs of those followers, and we could get more information 
# about them if we want using the lookup_users function. If we were interested in 
# creating a larger social network analysis dataset centered around Sanders, 
# we could scrape the followers of his followers within a loop.


rate_limits<-rate_limit()
head(rate_limits[,1:4])

get_trends("New York")


#########3

#load list of twitter handles for elected officials
elected_officials<-read.csv("https://cbail.github.io/Senators_Twitter_Data.csv", stringsAsFactors = FALSE)

head(elected_officials)

#create empty container to store tweets for each elected official
elected_official_tweets<-as.data.frame(NULL)

for(i in 1:nrow(elected_officials)){
  
  #pull tweets
  tweets<-get_timeline(elected_officials$twitter_id[i], n=100)
  
  #populate dataframe
  elected_official_tweets<-rbind(elected_official_tweets, tweets)
  
  #pause for five seconds to further prevent rate limiting
  Sys.sleep(1)
  
  #print number/iteration for debugging/monitoring progress
  print(i)
}
load(url("https://cbail.github.io/Elected_Official_Tweets.Rdata"))

#rename twitter_id variable in original dataset in order to merge it with tweet dataset
colnames(elected_officials)[colnames(elected_officials)=="twitter_id"]<-"screen_name"
for_analysis<-left_join(elected_official_tweets, elected_officials)

# Let’s inspect the outcome measure:
hist(for_analysis$retweet_count)

# Next, because our data is so skewed, we could use something like a negative 
# binomial regression model to examine the association between the various 
# predictors in our data and the outcome. To do this we will use the glm.nb 
# function from the MASS package:

summary(glm.nb(favorite_count~
                 party+
                 followers_count+
                 statuses_count+
                 gender, 
               data=for_analysis))

## Working with Timestamps
head(for_analysis$created_at)

# To manage these types of string variables that describe dates, 
# it is often very useful to convert them into a variable of class “date.” 
# There are several ways to do this in R, but here is the way to do it using the as.Date function in base R.

for_analysis$date<-as.Date(for_analysis$created_at, format="%Y-%m-%d")
head(for_analysis$date)

# Now, we can subset the data using conventional techniques. 
# For example, if we wanted to only look at tweets for August, we could do this:
  august_tweets<-for_analysis[for_analysis$date>"2018-07-31"&
                                for_analysis$date<"2018-09-01",]
  
  
  ### class notes
# making requests through R with httr
  r <- GET("https://www.iea.org/policies")
r
