# Create an account on shiniapps.io to deploy your first app. This
# procedure together will take approximately 10 minutes.

# - Go to: https://www.shinyapps.io/admin/#/signup
# 
# - Create an account (name + password). You can use whatever email
#   you like.
# 
# - Fill in your tokens (accountname + token + secret) in the code
#   below
# 

library(igraph)
library(shiny)
library(data.table)
library(rsconnect) # open libraries that you will use today

setAccountInfo(name='patriciafonseca',
               token='BC3385D6691663D1FEC7C7D1ABCC14E9',
               secret='zchzVnygYCzb/OuT3TeqtYdlYJ3P5Nua7L15FP0Y')
deployApp('/Users/patriciafonseca/Desktop/Mestrado Nova SBE/Aulas/S2/T1/Network Analytics/Group assignment')

# Loading CSV file

# Load data from csv file 
setwd("/Users/patriciafonseca/Desktop/Mestrado Nova SBE/Aulas/S2/T1/Network Analytics/Group assignment/Project")
dt.influencers <- fread("instagram_global_top_1000.csv") 

# Save dt.movie.actor. Next time you can simply call the load function (below)
save(dt.influencers, file="influencers.RData")

# Load previously saved dt.movie.actor. You can
# start in this line if you have previously saved these data.
load("influencers.RData")

# Remove entries in which influencers have no category  
dt.influencers <- dt.influencers[!(Category == "")]

for (row in 1:nrow(dt.influencers)) {
  
  dt.influencers$Category[row] <- as.list((strsplit(as.character(dt.influencers$Category[row]), split = "\\|")))
  
}

View(dt.influencers)


