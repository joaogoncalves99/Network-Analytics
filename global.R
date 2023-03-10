library(data.table)
library(igraph)
library(ggplot2)
library(shiny)

#Page with network descriptive stats

# Table with info about centralities (degree, eigenvector, etc) - only table changes with user input.
# Histogram with degree distribution: changes according to if it is a bipartite network (one thing on each side - countries and influencers, countries and categories) or projected network (countries based on favourite categories, countries based on favourite influencers, Categories, etc)
# If talking about influencers, histogram also changes according to minimum number of followers
# Histogram changes according to minimum weight
#Add option to eliminate nodes with no connections

# Page with network exploration: highlight influencers or countries, etc


# Define server logic ----
server <- function(input, output) {
  
  output$overall.network <- renderUI({
    stud <- student.from.key(input$student.key)
    assign <- input$assignment
    g.sim <- graph.data.frame(dt.similarity.pairs[order(z_score)], directed=FALSE)
    diplay <- list()
    if (length(stud) > 0) {
      if (length(similarity.score(stud, assign)) > 0) {
        display <- list(h2(paste0('Hello ', student.name(stud), '!')),
                        br())
      } else {
        display <- list(h2(paste0('Hello ', student.name(stud), '!')),
                        br(),
                        p('It seems that you did not hand in this assignment.'))
      }} else {
        display <- list(h2('Hey there!'),
                        br(),
                        p('It appears you have entered an invalid student key.'))
      }
    list(display,
         list(
           selectInput("overall.min.z.score", "Show links with at least the level of suspiciousness:",
                       c("Above average" = 0,
                         "Hmm..." = 1.64,
                         "Yes" = 1.96,
                         "Very!" = 2.57)),
           renderPlot(overall.similarity.plot(g.sim, stud, assign,
                                              overall.min.z.score = input$overall.min.z.score), height=600, width=600)
         ))
  })
}
  

server

