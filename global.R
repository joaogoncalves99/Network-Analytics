# Loading CSV file

# Load data from csv file 
setwd("/Users/joaogoncalves/Documents/Universidade/Mestrado/2nd Semester/T3/Network Analytics/Assignments/Shiny App/Network-Analytics")
#dt.influencers <- fread("instagram_global_top_1000.csv") 

# Save dt.movie.actor. Next time you can simply call the load function (below)
#save(dt.influencers, file="influencers.RData")

# Load previously saved dt.movie.actor. You can
# start in this line if you have previously saved these data.
load("influencers.RData")

library(data.table)
library(igraph)
library(ggplot2)
library(shiny)


#dt.influencers <- dt.influencers[order(-Followers)][1:15,]

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

# print the resulting dataset
#View(dt.influencers.new)

# create function that when user inputs @ of influencer, it gets the account name - we can show the degree measures for that account using this input - we also need this for network model
title.from.account <- function(influencer.account) {
  dt.influencers.new[account == influencer.account, Title]
}


#E(g)

#unique(dt.influencers.new$Category)

#View(dt.influencers.new)
