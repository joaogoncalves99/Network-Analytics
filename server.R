library(data.table)
library(igraph)
library(ggplot2)
library(shiny)






network.plot <- function(to.delete, foll, top){
  dt.influencers.new <- dt.influencers.new[order(-Followers)][1:top,]
  dt.influencers.new <- dt.influencers.new[Followers >= foll,]
  
  categories <- dt.influencers.new[, list(name=unique(Category), type=TRUE)]
  influencers <- dt.influencers.new[, list(name=unique(Account), type=FALSE)]
  all.vertices <- rbind(categories, influencers)
  g <- graph.data.frame(dt.influencers.new[, list(Category, Account)], directed=FALSE, vertices=all.vertices)
  g.influencers <- bipartite_projection(g)$proj1
  
  if(to.delete == TRUE){
    g.influencers <- induced.subgraph(g.influencers, which(round(degree(g.influencers, normalize=TRUE), 2) != 0))
  }  
  
  # Generate x and y values for the sine function
  #x <- seq(-input$x, input$x, length.out = 100)
  #y <- sin(x)
  
  # Plot the sine function
  #renderPlot(plot(x, y, type = "l", main = "Sine Function", xlab = "x", ylab = "sin(x)"))
  plot(g.influencers,vertex.shape="circle", 
                  vertex.color = "#FD1D91", 
                  vertex.frame.color = "#FD1D91", 
                  vertex.size = 20, 
                  vertex.label.family = "sans",
                  vertex.label.cex = 1)

}


# Define server logic ----
server <- function(input, output) {
  
  # Define the output plot
  output$influencer.network <- renderUI({
    n.followers <- input$nfollowers.input * 1000000
    top.followers <- input$top.followers.input

    if(input$no.connection.input){
      renderPlot(network.plot(TRUE, foll = n.followers, top = top.followers))
    }else{
      renderPlot(network.plot(FALSE, foll = n.followers, top = top.followers))
    }
    
  })
  
  output$summary_data <- renderTable({
        dt.general
        #falta be sure das categorias
    })
    
    output$Top_10_Categories <- renderTable({
        
    })
    
    output$Top_10_influencers <- renderTable({
        top10$Title
    })
    
    output$Top_10_contry_engage <- renderTable({
        top10country$unique
        
    })
  
  output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        if (input$tops=="Top 10") {x    <- top10$Followers}
        if (input$tops=="All") {x    <- dt.all$Followers}
        if (input$tops=="Top 50") {x    <- top50$Followers}
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab=input$tops, 
             main=paste("Histogram for Followers", input$tops))
    })
    
    output$summary <- renderPrint({
        if (input$tops=="Top 10") {x    <- top10$Followers}
        if (input$tops=="All") {x    <- dt.all$Followers}
        if (input$tops=="Top 50") {x    <- top50$Followers}
    
        summary(x)
        
    })
  
}
  

server

