library(data.table)
library(igraph)
library(ggplot2)
library(shiny)

ui <- fluidPage(
  
  tags$div(
    style = "padding: 10px; text-align: center;",
    tags$h1("Instagram Shiny App", style = "color: #FD1D91; text-shadow: 1px 1px #333;")
  ),
  
  # Define a navbar with links to two pages
  navbarPage(
    "", 
    
    tabPanel("Home",
         
             #ADD THINGS ON HOME PAGE HERE
                 
    ),
    
    tabPanel("Descriptive Statistics",
             
            sidebarLayout(
              sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 100,
                        value = 20),
            radioButtons("tops", "Choose the Top Influencer you want to look at", 
                         choices=c("Top 10", "Top 50", "All"),
                         selected="Top 10"),
            selectInput("countries",
                        label= "Select which country you want to analyse", 
                        choices=dt.influencers.combined.country$unique, 
                        selected="United States of America",
                        multiple=F)
                 
               ),
               
               # Show a plot of the generated distribution
            mainPanel(
            tabsetPanel(type = "tab",
                        tabPanel("General Data", 
                                 h3("Data Outlook"),
                                 tableOutput("summary_data"),
                                 h3("Top 10 Influencers"),
                                 tableOutput("Top_10_influencers"),
                                 h3("Top 10 Influencers Categories"),
                                 tableOutput("Top_10_Categories"),
                                 h3("Top 10 Country Engagement"),
                                 tableOutput("Top_10_contry_engage")),
                                 
                        tabPanel("Summary Statistics",
                                 plotOutput("distPlot"),
                                 verbatimTextOutput("summary")),
                                 
                        tabPanel("All about your Influencer"), 
                                 
                        tabPanel("Get more Specific")
               )
             )
             
    ),
    
    tabPanel("Network Exploration",
      
      # Sidebar 
      sidebarLayout(
        sidebarPanel(
         
          h5("Here you can change the parameters of the network you would like to see."),
         
          radioButtons("network.key",
                      label = "Type of Network",
                      choices = c("Bipartite Network", "Projected Network")),
         
          selectizeInput("category.input", "Select the Category:",
                        choices = unique(dt.influencers.new$Category)
          ),
         
          sliderInput("top.followers.input", "Top Followers to Display:",
                      min = 0, max = 1000, value = 0, step = 25),
          
          sliderInput("nfollowers.input", "Minimum Number of Followers (milions):",
                     min = 0, max = 470, value = 0, step = 25),
          
          checkboxInput("no.connection.input", "Delete nodes which have no connection to other nodes", value = FALSE),
         
        ),
       
       mainPanel(
          tabsetPanel(
            tabPanel("Summary", htmlOutput("student.summary")
                     
            ),
            
            tabPanel("Your Network", htmlOutput("influencer.network"),
                     #numericInput(inputId = "x", label = "Enter a number:", value = 1),
                     
                     # Define the output plot
                     plotOutput(outputId = "sine_plot")
                    #WRITE HERE FOR THE TAB YOUR NETWORK
            ),
            
            tabPanel("Overall Network", htmlOutput("overall.network")
                     #WRITE HERE FOR THE TAB OVERALL NETWORK
                     
            )
          )
        )
      )
    )
  ),
  
  
  
)

ui
