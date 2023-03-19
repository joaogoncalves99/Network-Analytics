library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(wordcloud2)


# Define server
server <- function(input, output) {
  
  output$my_table <- renderTable({
    
    my_data <- data.frame("Student Name" = c("Inês", "João", "Julia", "Maria", "Patrícia"), 
                          "Student ID" = c("54045", "51268", "53781", "54350", "53004"))
    colnames(my_data) <- NULL
    rownames(my_data) <- NULL
    my_data
    
  })
  
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
    
    "In this tab we have created a popularity rank for all the influencers in the dataset, grouping them in 5 
    different categories from 1 to 5, being 5 the highest level of popularity. Besides the number of top 
    influencers belonging to rank 5, here we can see the number of followers, the authentic engagement (number 
    of likes and comments that, on average, come from 'real people' and influencers per post) and the average 
    engagement (total number of likes and comments of an account divided by its number of followers) of each 
    rank. You can explore this descriptive statistics 
    using the drop-down menu on the side."
    
  })
  
  output$plot <- renderPlot({
    
    my_subset <- subset(dt.influencers, select = c("Title", "Category","Followers","Audience Country","Authentic engagement"))
    my_subset$Followers <- my_subset$Followers/1000000
    my_subset$Authentic_engagement <- my_subset$`Authentic engagement`/1000000
    
    
    if(input$tops == "Top 50"){
      my_subset <- my_subset[order(-dt.influencers$Followers), ][1:50,]
    }else if(input$tops == "Top 100"){
      my_subset <- my_subset[order(-dt.influencers$Followers), ][1:100,]
    }else{
      my_subset <- my_subset[order(-dt.influencers$Followers), ]
    }
    
    my_subset$Category <- sapply(my_subset$Category, function(x) gsub("[c()]", "", x[1]))
    
    if (input$statistics == "Followers Distribution") {
      g1 <- ggplot(my_subset, aes(x =my_subset$Followers, fill ="#FD1D91")) + 
        geom_histogram(bins=input$bins)+
        xlab("Number of Followers (M)")+ ylab("Frequency")+
        ggtitle(input$statistics)+
        theme(plot.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"))
      plot(g1)
    }
    
    
    if (input$statistics == "Followers Distribution by Country") {
      g1 <- ggplot(my_subset, aes(x = my_subset$Followers, y=my_subset$`Audience Country`,color ="#FD1D91" )) + 
        geom_point()+
        xlab("Followers") + ylab("Audience Country") +
        ggtitle(input$statistics) +
        theme(plot.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"))
      plot(g1)
      
    }
    
    
    
    if (input$statistics == "Followers Distribution by Category") {
      g1 <- ggplot(my_subset, aes(x = my_subset$Followers, y=my_subset$Category, color ="#FD1D91" )) + 
        geom_point()+
        xlab("Followers") + ylab("Category") +
        ggtitle(input$statistics) +
        theme(plot.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"))
      plot(g1)
    }
    
    if (input$statistics == "Engagement Distribution") {
      g1 <- ggplot(my_subset, aes(x = my_subset$`Authentic engagement`,fill ="#FD1D91")) + 
        geom_histogram(bins=input$bins)+
        xlab("Authentic Engagement (M)")+ ylab("Frequency")+
        ggtitle(input$statistics)+
        theme(plot.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"))
      plot(g1)
    }
    
    
    
    if (input$statistics == "Engagement Distribution by Country") {
      g1 <- ggplot(my_subset, aes(x = my_subset$`Authentic engagement`, y=my_subset$`Audience Country`,color ="#FD1D91")) + 
        geom_point()+
        xlab("Authentic Engagement") + ylab("Country") +
        ggtitle(input$statistics) +
        theme(plot.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"))
      plot(g1)
    }
    
    if (input$statistics == "Engagement Distribution by Category") {
      g1 <- ggplot(my_subset, aes(x = my_subset$`Authentic engagement`, y=my_subset$Category,color ="#FD1D91")) + 
        geom_point()+
        xlab("Authentic Engagement") + ylab("Category") +
        ggtitle(input$statistics) +
        theme(plot.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"))
      plot(g1)
    }
  })
  
  output$influencers.metrics <- renderDataTable({
    
    subset1 <- subset(dt.influencers, select = c("Account", "Title", "Category","Followers","Authentic engagement", "rank"))
    
    subset1
    
  })
  
  output$wordcloud.metrics <- renderWordcloud2({            #Making a word cloud for the categories in each country
    country.cat <- input$country.cat
    dt.aux2 <- dt.influencers.new[Audience_country == country.cat, ]
    category.frequency <- as.data.frame(table(dt.aux2$Category))
    
    wordcloud2(category.frequency, size = 1, color = "random-light", backgroundColor = "#f0f0f0")
    
  })
  
  output$categories.metrics <- renderDataTable({
    
    country.cat <- input$country.cat
    dt.aux1 <- dt.influencers.new[Audience_country ==  country.cat, ]
    
    #Getting the number of mentions of each category in the dataframe
    category.freq <- as.data.frame(table(dt.aux1$Category))
    category.freq <- rename(category.freq, Category = Var1, Frequency = Freq)
    #Getting the mean authentic engagement for each category
    mean.engage <- dt.aux1[, .(mean_engagement = mean(Authentic_engagement)), by = Category]
    
    mean.engage <- mean.engage[order(Category)]  #Ordering the dataframe alphabetically to match the previously created table
    
    merged.cat <- merge(category.freq, mean.engage, by ="Category")  #Merging the all the two tables
    
    #Getting the minimum authentic engagement for each category
    min.engage <- dt.influencers.new[, .(min_engagement = min(Authentic_engagement)), by = Category]
    
    merged.cat <- merge(merged.cat, min.engage, by ="Category")  #Merging the two tables
    
    #Getting the maximum authentic engagement for each category
    max.engage <- dt.influencers.new[, .(max_engagement = max(Authentic_engagement)), by = Category]
    
    merged.cat <- merge(merged.cat, max.engage, by ="Category")  #Merging the two tables
    
    #Getting the standard deviation for the authentic engagement for each category
    stdv.engage <- aggregate(dt.influencers.new$Authentic_engagement, by = list(Category = dt.influencers.new$Category), FUN = sd)
    stdv.engage <- rename(stdv.engage, Category = Category, Stdv_Engagement = x)
    stdv.engage$Stdv_Engagement <- ifelse(is.na(stdv.engage$Stdv_Engagement), 0, stdv.engage$Stdv_Engagement)
    
    merged.cat <- merge(merged.cat, stdv.engage, by ="Category")  #Merging the two tables
    merged.cat
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
         xlab = "Degree Centrality",
         ylab = "Number of Influencers",
         main = "Histogram Category")
    }else{ #WRITE HERE THE GRAPH FOR COUNTRY
      hist(degree(graph), breaks = bins, col = "#FD1D91", border = "white",
           xlab = "Degree Centrality",
           ylab = "Number of Influencers",
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
  
  output$prediction.network <- renderVisNetwork({
    top.followers <- input$top.influencers
    
    graph <- network.graph(TRUE, foll = 0, top = top.followers,TRUE)
    
    my.layout <- layout_nicely(graph)
    m.predicted.edges <- 
      as.matrix(cocitation(graph) * (1-get.adjacency(graph)))
    g.predicted.edges <- 
      graph_from_adjacency_matrix(m.predicted.edges, 
                                  mode = "undirected", 
                                  weighted = TRUE)
    E(g.predicted.edges)$width <- 
      E(g.predicted.edges)$weight * 2
    
    visGDF <- toVisNetworkData(g.predicted.edges)
    
    visNetwork(visGDF$nodes, visGDF$edges) %>%
      visOptions(highlightNearest = TRUE) %>%
      visEdges(color = list(color = "black", highlight = "#00FFFF")) %>%
      visNodes(color = list(border = "#FD1D91", background = "#FD1D91", highlight = list(background = "#FFFF00")))
  })
  
  output$jaccardi.index <- renderDataTable({
    top.followers <- input$top.influencers.jaccard
    min <- input$interval.jaccard[1]
    max <- input$interval.jaccard[2]
    
    dt.aux <- dt.influencers.new[Followers >= 0, .N, by=list(Account, Followers)][order(-Followers)][1:top.followers, Account]
    dt.influencers.new <- dt.influencers.new[Account %in% dt.aux,]
    
    cat.count <- dt.influencers.new[, list(name=unique(Category), type=TRUE)]
    influencers <- dt.influencers.new[, list(name=unique(Account), type=FALSE)]
    all.vertices <- rbind(cat.count, influencers)
    g <- graph.data.frame(dt.influencers.new[, list(Category, Account)], directed=FALSE, vertices=all.vertices)
    
    g.proj <- bipartite_projection(g)$proj1
    
    aux.jaccard <- unique(dt.influencers.new$Account)
    dt.aux.jaccard <- data.frame(Account = aux.jaccard)
    
    combs <- combn(dt.aux.jaccard$Account, 2)
    pairs <- t(combs)
    pairs <- data.table(pairs)
    pairs$similarity <- NA
    
    for (i in seq_along(pairs$V1)) {
      # compute the similarity between the pair of values
      sim <- similarity(g.proj, v = c(pairs$V1[i], pairs$V2[i]), method = "jaccard")[1, 2]
      # store the similarity in the column
      pairs$similarity[i] <- sim
    }
    
    pairs <- rename(pairs, Influencer_1 = V1)
    pairs <- rename(pairs, Influencer_2 = V2)
    
    pairs[pairs$similarity >= min & pairs$similarity <= max ,]
    
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

