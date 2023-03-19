library(igraph)
library(shiny)
library(data.table)
library(rsconnect) # open libraries that you will use today

rsconnect::setAccountInfo(name='joaogoncalves99', 
                          token='A676FBD2BAE4FAB90A6D9605D9530BBE', 
                          secret='R/4pplvYqI+4Pt4VhZmHMP86SKBXaL6VR3CrGRl1')


rsconnect::deployApp('/Users/joaogoncalves/Downloads/Network-Analytics-joao')
