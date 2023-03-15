library(data.table)
library(igraph)
library(ggplot2)
library(shiny)

# Define server
server <- function(input, output) {

  output$general.statistics <- renderTable({
  
    
    #Creating a table with the number of followers, the top influencer, the authentic engagement and the engagement average for each country in the dataset 
    dt.aux <- subset(dt.influencers, rank == input$rank )
    
    number.followers.country <- sum(dt.aux$Followers)
    
    number.followers.authentic.eng <- sum(dt.aux$`Authentic engagement`)
    
    number.followers.average.eng <- sum(dt.aux$`Engagement avg`)
      
    dt.descriptive.statistics <- data.table(Statistic = character(),
                                            Value = numeric())

    dt.descriptive.statistics <- rbind(dt.descriptive.statistics,
                                       list("Number of Top Influencers", nrow(dt.aux)),
                                       list("Number of Followers", number.followers.country),
                                       list("Authentic Engagement", number.followers.authentic.eng),
                                       list("Average Engagement", number.followers.average.eng)
                                  )
    dt.descriptive.statistics
   
  })
  
  output$general.text <- renderText({
    
    "This tab provides an overview on the number of followers, influencer categories, the authentic engagement and engagement average for each audience country in our dataset by using the button on the side."
    
  })
  
  output$network.statistics <- renderTable({
    #Store the number of followers parameter from the input
    n.followers <- input$nfollowers.input * 1000000
    #Store the top followers parameter the from input
    top.followers <- input$top.followers.input
    #Store the choices provided by the input
    two.choices <- input$two.choices
    
    if(input$no.connection.input){
      if (two.choices == "Category"){
        graph <- network.graph(TRUE, foll = n.followers, top = top.followers,TRUE)
      }else{
        graph <- network.graph(TRUE, foll = n.followers, top = top.followers,FALSE)
      }
    }else{
      if (two.choices == "Category"){
        graph <- network.graph(FALSE, foll = n.followers, top = top.followers,TRUE)
      }else{
        graph <- network.graph(FALSE, foll = n.followers, top = top.followers,FALSE)
      }
    }
      
    dt.descriptive.statistics <- data.table(Statistic = character(),
                                            Value = numeric())
    
    dt.descriptive.statistics <- rbind(dt.descriptive.statistics,
                                       list("Number of Nodes", vcount(graph)),
                                       list("Number of Edges", ecount(graph)),
                                       list("Average Clustering Coefficient", transitivity(graph, type = "average")),
                                       list("Average Path Length", average.path.length(graph)),
                                       list("Diameter", diameter(graph)))
    dt.descriptive.statistics
  })
  
  output$histogram.statistics <- renderPlot({
    
    n.followers <- input$nfollowers.input * 1000000
    top.followers <- input$top.followers.input
    bins <- input$bins.input
    #Store the choices provided by the input
    two.choices <- input$two.choices
    
    if(input$no.connection.input){
      if (two.choices == "Category"){
        graph <- network.graph(TRUE, foll = n.followers, top = top.followers,TRUE)
      }else{
        graph <- network.graph(TRUE, foll = n.followers, top = top.followers,FALSE)
      }
    }else{
      if (two.choices == "Category"){
        graph <- network.graph(FALSE, foll = n.followers, top = top.followers,TRUE)
      }else{
        graph <- network.graph(FALSE, foll = n.followers, top = top.followers,FALSE)
      }
    }
    
    if (two.choices == "Category"){ #WRITE HERE THE GRAPH FOR CATEGORY
    hist(degree(graph), breaks = bins, col = "#FD1D91", border = "white",
         xlab = "eoifjiwe",
         ylab = "ewiuhfwoehfwe",
         main = "Histogram Category")
    }else{ #WRITE HERE THE GRAPH FOR COUNTRY
      hist(degree(graph), breaks = bins, col = "#FD1D91", border = "white",
           xlab = "eoifjiwe",
           ylab = "ewiuhfwoehfwe",
           main = "Histogram Country")
    }
  })
  
  output$network.metrics <- renderDataTable({
    #Store the number of followers parameter from the input
    n.followers <- input$nfollowers.input * 1000000
    #Store the top followers parameter the from input
    top.followers <- input$top.followers.input
    #Store the choices provided by the input
    two.choices <- input$two.choices
    
    if(input$no.connection.input){
      if (two.choices == "Category"){
        graph <- network.graph(TRUE, foll = n.followers, top = top.followers,TRUE)
      }else{
        graph <- network.graph(TRUE, foll = n.followers, top = top.followers,FALSE)
      }
    }else{
      if (two.choices == "Category"){
        graph <- network.graph(FALSE, foll = n.followers, top = top.followers,TRUE)
      }else{
        graph <- network.graph(FALSE, foll = n.followers, top = top.followers,FALSE)
      }
    }
    
    #Calculate Degree Centrality again after removing the movies
    dc <- round(degree(graph, normalize = TRUE),2)
    #Calculate Closeness Centrality
    cc <- round(closeness(graph, normalize = TRUE), 2)
    #Calculate Betweenness Centrality
    bc <- round(betweenness(graph, normalize = TRUE), 2)
    #Calculate Eigenvector Centrality
    ec <- round(evcent(graph)$vector, 2)
    
    #Create a data frame with all the centrality measures
    centrality.measures <- data.frame(account = names(bc), degree_centrality = dc, closeness_centrality = cc, betweenness_centrality = bc, eigenvector_centrality = ec)
    
    centrality.measures
    
  })
  
  output$country.network <- renderVisNetwork({
    n.followers <- input$nfollowers.input * 1000000
    top.followers <- input$top.followers.input
    country.choices <- input$countries.choices
    
    if(input$no.connection.input){
      graph <- network.graph.choices(TRUE, foll = n.followers, top = top.followers, choices = country.choices, FALSE)
    }else{
      graph <- network.graph.choices(FALSE, foll = n.followers, top = top.followers, choices = country.choices, FALSE)
    }
    
    visGDF <- toVisNetworkData(graph)
    
    visNetwork(visGDF$nodes, visGDF$edges) %>%
      visOptions(highlightNearest = TRUE) %>%
      visEdges(color = list(color = "black", highlight = "orange")) %>%
      visNodes(color = list(border = "#FD1D91", background = "#FD1D91", highlight = list(background = "#962fbf")))
    
  })
  
  output$category.network <- renderVisNetwork({
    n.followers <- input$nfollowers.input * 1000000
    top.followers <- input$top.followers.input
    category.choices <- input$category.choices
    
    if(input$no.connection.input){
      graph <- network.graph.choices(TRUE, foll = n.followers, top = top.followers, choices = category.choices, TRUE)
    }else{
      graph <- network.graph.choices(FALSE, foll = n.followers, top = top.followers, choices = category.choices, TRUE)
    }
    
    visGDF <- toVisNetworkData(graph)

    visNetwork(visGDF$nodes, visGDF$edges) %>%
      visOptions(highlightNearest = TRUE) %>%
      visEdges(color = list(color = "black", highlight = "#00FFFF")) %>%
      visNodes(color = list(border = "#FD1D91", background = "#FD1D91", highlight = list(background = "#FFFF00")))
    
  })
}

#These function returns the a graph based on the sidebar conditions
network.graph <- function(to.delete, foll, top, type){
  
  dt.aux <- dt.influencers.new[Followers >= foll, .N, by=list(Account, Followers)][order(-Followers)][1:top, Account]
  dt.influencers.new <- dt.influencers.new[Account %in% dt.aux,]
  
  
  if(type){
    cat.count <- dt.influencers.new[, list(name=unique(Category), type=TRUE)]
    influencers <- dt.influencers.new[, list(name=unique(Account), type=FALSE)]
    all.vertices <- rbind(cat.count, influencers)
    g <- graph.data.frame(dt.influencers.new[, list(Category, Account)], directed=FALSE, vertices=all.vertices)
  }else{
    cat.count <- dt.influencers.new[, list(name=unique(Audience_country), type=TRUE)]
    influencers <- dt.influencers.new[, list(name=unique(Account), type=FALSE)]
    all.vertices <- rbind(cat.count, influencers)
    g <- graph.data.frame(dt.influencers.new[, list(Audience_country, Account)], directed=FALSE, vertices=all.vertices)
  }
  
  g.proj <- bipartite_projection(g)$proj1
  
  if(to.delete == TRUE){
    g.proj <- induced.subgraph(g.proj, which(round(degree(g.proj, normalize=TRUE), 2) != 0))
  } 
  
  return(g.proj)
  
}

#Here the parameter type is a Boolean, if its true means that we are choosing categories, if not we are choosing Country
network.graph.choices <- function(to.delete, foll, top, choices, type){
  dt.aux <- dt.influencers.new[Followers >= foll, .N, by=list(Account, Followers)][order(-Followers)][1:top, Account]
  dt.influencers.new <- dt.influencers.new[Account %in% dt.aux,]
  
  
  if (length(choices) >= 1){
    if(type){
      dt.influencers.new <- dt.influencers.new[dt.influencers.new$Category %in% c(choices),]
    }else{
      dt.influencers.new <- dt.influencers.new[dt.influencers.new$Audience_country %in% c(choices),]
    }
  }
  
  if(type){
    cat.count <- dt.influencers.new[, list(name=unique(Category), type=TRUE)]
    influencers <- dt.influencers.new[, list(name=unique(Account), type=FALSE)]
    all.vertices <- rbind(cat.count, influencers)
    g <- graph.data.frame(dt.influencers.new[, list(Category, Account)], directed=FALSE, vertices=all.vertices)
  }else{
    cat.count <- dt.influencers.new[, list(name=unique(Audience_country), type=TRUE)]
    influencers <- dt.influencers.new[, list(name=unique(Account), type=FALSE)]
    all.vertices <- rbind(cat.count, influencers)
    g <- graph.data.frame(dt.influencers.new[, list(Audience_country, Account)], directed=FALSE, vertices=all.vertices)
  }
  
  g.proj <- bipartite_projection(g)$proj1
  
  if(to.delete == TRUE){
    g.proj <- induced.subgraph(g.proj, which(round(degree(g.proj, normalize=TRUE), 2) != 0))
  }
  
  return(g.proj)
  
}

server

