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



