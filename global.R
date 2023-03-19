# Loading CSV file

# Load data from csv file 

load("influencers.RData")

library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(dplyr)

# split the category column by "|"
dt.influencers$Category <- strsplit(as.character(dt.influencers$Category), "\\|")
# create a new dataframe with categories as separate rows
dt.influencers.new <- data.table(Account = rep(dt.influencers$Account, sapply(dt.influencers$Category, length)),
                                 Title = rep(dt.influencers$Title, sapply(dt.influencers$Category, length)),
                                 Link = rep(dt.influencers$Link, sapply(dt.influencers$Category, length)),
                                 Category = unlist(dt.influencers$Category),
                                 Followers = rep(dt.influencers$Followers, sapply(dt.influencers$Category, length)), 
                                 Audience_country = rep(dt.influencers$`Audience Country`, sapply(dt.influencers$Category, length)),
                                 Authentic_engagement = rep(dt.influencers$`Authentic engagement`, sapply(dt.influencers$Category, length)),
                                 Engagement_avg = rep(dt.influencers$`Engagement avg`, sapply(dt.influencers$Category, length)))

dt.influencers <- dt.influencers %>% 
  mutate(rank = ntile(Followers, n = 5)) 
