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

rsconnect::setAccountInfo(name='joaogoncalves99', 
                          token='A676FBD2BAE4FAB90A6D9605D9530BBE', 
                          secret='R/4pplvYqI+4Pt4VhZmHMP86SKBXaL6VR3CrGRl1')


rsconnect::deployApp('/Users/joaogoncalves/Documents/Universidade/Mestrado/2nd Semester/T3/Network Analytics/Assignments/Shiny App/Network-Analytics')
