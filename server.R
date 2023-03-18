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
  
  output$all <- DT::renderDataTable({
        DT::datatable({
            data <- dt.influencers
            if (input$countries != "All") {
                data <- data[data$Audience.Country == input$countries, ]
            }
            data
        })
    })
    
    output$plot <- renderPlot({
        if (input$statistics == "Followers Distribution" & input$tops =="Top 50" ) {
            g1 <- ggplot(dt.influencers.top50, aes(x =dt.influencers.top50$Followers)) + 
                geom_histogram(bins=input$bins)+
                xlab("Number of Followers (M)")+ ylab("Frequency")+
                ggtitle(input$statistics)+
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
   
        if (input$statistics == "Followers Distribution" & input$tops =="Top 100" ) {
            g1 <- ggplot(dt.influencers.top100, aes(x =dt.influencers.top100$Followers)) + 
                geom_histogram(bins=input$bins)+
                xlab("Number of Followers (M)")+ ylab("Frequency")+
                ggtitle(input$statistics)+
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Followers Distribution" & input$tops =="All" ) {
            g1 <- ggplot(dt.influencers, aes(x =dt.influencers$Followers)) + 
                geom_histogram(bins=input$bins)+
                xlab("Number of Followers (M)")+ ylab("Frequency")+
                ggtitle(input$statistics)+
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Followers Distribution by Country" & input$tops == "Top 50") {
            g1 <- ggplot(dt.influencers.top50, aes(x = dt.influencers.top50$Followers, y=dt.influencers.top50$Audience.Country )) + 
                geom_point() +
                xlab("Influencers") + ylab("Audience Country") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
            
        }
        
        if (input$statistics == "Followers Distribution by Country" & input$tops == "Top 100") {
            g1 <- ggplot(dt.influencers.top100, aes(x = dt.influencers.top100$Followers, y=dt.influencers.top100$Audience.Country )) + 
                geom_point()+
                xlab("Influencers") + ylab("Audience Country") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
            
        }
        
        if (input$statistics == "Followers Distribution by Country" & input$tops == "All") {
            g1 <- ggplot(dt.influencers, aes(x = dt.influencers$Followers, y=dt.influencers$Audience.Country )) + 
                geom_point()+
                xlab("Influencers") + ylab("Audience Country") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
            
        }
        
        if (input$statistics == "Followers Distribution by Category" & input$tops == "Top 50") {
            g1 <- ggplot(dt.influencers.top50, aes(x = dt.influencers.top50$Followers, y=dt.influencers.top50$Category )) + 
                geom_point()+
                xlab("Influencers") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Followers Distribution by Category" & input$tops == "Top 100") {
            g1 <- ggplot(dt.influencers.top100, aes(x = dt.influencers.top100$Followers, y=dt.influencers.top100$Category )) + 
                geom_point()+
                xlab("Influencers") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Followers Distribution by Category" & input$tops == "All") {
            g1 <- ggplot(dt.influencers, aes(x = dt.influencers$Followers, y=dt.influencers$Category )) + 
                geom_point()+
                xlab("Influencers") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution" & input$tops == "Top 50") {
            g1 <- ggplot(dt.influencers.top50, aes(x = dt.influencers.top50$Authentic.Engagement)) + 
                geom_histogram(bins=input$bins)+
                xlab("Engagement (M)")+ ylab("Frequency")+
                ggtitle(input$statistics)+
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution" & input$tops == "Top 100") {
            g1 <- ggplot(dt.influencers.top100, aes(x = dt.influencers.top100$Authentic.Engagement)) + 
                geom_histogram(bins=input$bins)+
                xlab("Engagement (M)")+ ylab("Frequency")+
                ggtitle(input$statistics)+
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution" & input$tops == "All") {
            g1 <- ggplot(dt.influencers, aes(x = dt.influencers$Authentic.Engagement)) + 
                geom_histogram(bins=input$bins)+
                xlab("Engagement (M)")+ ylab("Frequency")+
                ggtitle(input$statistics)+
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution by Country" & input$tops == "Top 50") {
            g1 <- ggplot(dt.influencers.top50, aes(x = dt.influencers.top50$Authentic.Engagement, y=dt.influencers.top50$Audience.Country)) + 
                geom_point()+
                xlab("Engagement") + ylab("Coutry") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution by Country" & input$tops == "Top 100") {
            g1 <- ggplot(dt.influencers.top100, aes(x = dt.influencers.top100$Authentic.Engagement, y=dt.influencers.top100$Audience.Country)) + 
                geom_point()+
                xlab("Engagement") + ylab("Country") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution by Country" & input$tops == "All") {
            g1 <- ggplot(dt.influencers, aes(x = dt.influencers$Authentic.Engagement, y=dt.influencers$Audience.Country)) + 
                geom_point()+
                xlab("Engagement") + ylab("Country") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution by Category" & input$tops == "Top 50") {
            g1 <- ggplot(dt.influencers.top50, aes(x = dt.influencers.top50$Authentic.Engagement, y=dt.influencers.top50$Category)) + 
                geom_point()+
                xlab("Engagement") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution by Category" & input$tops == "Top 100") {
            g1 <- ggplot(dt.influencers.top100, aes(x = dt.influencers.top100$Authentic.Engagement, y=dt.influencers.top100$Category)) + 
                geom_point()+
                xlab("Engagement") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Engagement Distribution by Category" & input$tops == "All") {
            g1 <- ggplot(dt.influencers, aes(x = dt.influencers$Authentic.Engagement, y=dt.influencers$Category)) + 
                geom_point()+
                xlab("Engagement") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Country Engagement by Influencer Category" & input$tops == "Top 50") {
            g1 <- ggplot(dt.influencers.top50, aes(x = dt.influencers.top50$Audience.Country, y=dt.influencers.top50$Category)) + 
                geom_point()+
                xlab("Country Engagement") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Country Engagement by Influencer Category" & input$tops == "Top 100") {
            g1 <- ggplot(dt.influencers.top100, aes(x = dt.influencers.top100$Audience.Country, y=dt.influencers.top100$Category)) + 
                geom_point()+
                xlab("Country Engagement") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
        
        if (input$statistics == "Country Engagement by Influencer Category" & input$tops == "All") {
            g1 <- ggplot(dt.influencers, aes(x = dt.influencers$Audience.Country, y=dt.influencers$Category)) + 
                geom_point()+
                xlab("Country Engagement") + ylab("Category") +
                ggtitle(input$statistics) +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12, face = "bold"))
            plot(g1)
        }
    })
            
    output$stats <- renderPrint({
        output$stats <- renderPrint({
        
        if (input$statistics == "Followers Distribution" & input$tops =="Top 50" ) {
            data_1 <- dt.influencers.top50
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution" & input$tops =="Top 100" ) {
            data_1 <- dt.influencers.top100
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution" & input$tops =="All" ) {
            data_1 <- dt.influencers
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution by Country" & input$tops == "Top 50") {
            data_1 <- dt.influencers.top50
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution by Country" & input$tops == "Top 100") {
            data_1 <- dt.influencers.top100
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution by Country" & input$tops == "All") {
            data_1 <- dt.influencers
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution by Category" & input$tops == "Top 50") {
            data_1 <- dt.influencers.top50
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution by Category" & input$tops == "Top 100") {
            data_1 <- dt.influencers.top100
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Followers Distribution by Category" & input$tops == "All") {
            data_1 <- dt.influencers
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution" & input$tops == "Top 50") {
            data_1 <- dt.influencers.top50
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution" & input$tops == "Top 100") {
            data_1 <- dt.influencers.top100
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution" & input$tops == "All") {
            data_1 <- dt.influencers
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution by Country" & input$tops == "Top 50") {
            data_1 <- dt.influencers.top50
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution by Country" & input$tops == "Top 100") {
            data_1 <- dt.influencers.top100
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution by Country" & input$tops == "All") {
            data_1 <- dt.influencers
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution by Category" & input$tops == "Top 50") {
            data_1 <- dt.influencers.top50
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution by Category" & input$tops == "Top 100") {
            data_1 <- dt.influencers.top100
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Engagement Distribution by Category" & input$tops == "All") {
            data_1 <- dt.influencers
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Country Engagement by Influencer Category" & input$tops == "Top 50") {
            data_1 <- dt.influencers.top50
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Country Engagement by Influencer Category" & input$tops == "Top 100") {
            data_1 <- dt.influencers.top100
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
        
        if (input$statistics == "Country Engagement by Influencer Category" & input$tops == "All") {
            data_1 <- dt.influencers
            data_1_sum <- summary(data_1)
            print(data_1_sum)
        }
    })
  
}
  

server

