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
                     mainPanel(
                         tabsetPanel(
                             tabPanel("General Data",
                                          h2("General Descriptive Data",style = "color: #FD1D91;"),
                                          p("This page will provide some more general data descriptions related to the data that we analyzed"),
                                          tags$hr(),
                                          column(5,
                                                 selectInput("audience.country.input",
                                                             label= "Select which country you want to analyse", 
                                                             choices= unique(dt.influencers.new$Audience_country), 
                                                             selected="United States of America",
                                                             multiple=F),
                                                 
                                                 tableOutput("general.statistics")
                                                 
                                          ),
                                          column(7,textOutput("general.text")),
                                 ),
                                          
                             tabPanel("General Statistics",
                                      h3("General Descriptive Statistics", style = "color: #FD1D91;"),
                                      p("This page will provide the most important statistic metrics on the collected data"),
                                      conditionalPanel(
                                          condition = "input.tabsetPanel.tabPanel1 == 'Statistics'",
                                          sidebarPanel(
                                              sliderInput("bins",
                                                          "Number of bins:",
                                                          min = 10,
                                                          max = 1000,
                                                          value = 50),
                                              radioButtons("tops",
                                                           label="Select the range Influencers you want to look at",
                                                           choices=c("All", "Top 50", "Top 100"),
                                                           selected="All"),
                                              selectInput("statistics", "Select the statistics you want to look at", choices=c("Followers Distribution" , "Followers Distribution by Country", "Followers Distribution by Category",
                                              "Engagement Distribution", "Engagement Distribution by Country", "Engagement Distribution by Category", "Country Engagement by Influencer Category"))
                                          ),
                                              
                                    
                                
                                      mainPanel(
                                              plotOutput("plot"),
                                              verbatimTextOutput("stats")
                                          ),
 
                                          )
                                         
                                      ),
                            
                             
                             tabPanel("All about your Influencer",
                                      h3("All about your Influencer", style = "color: #FD1D91;")
                                      ), 
                             
                             tabPanel("Get more Specific",
                                      h3("Get more Specific", style = "color: #FD1D91;")
                                      ),
                             
                             tabPanel("Data",
                             h3("Data Set", style = "color: #FD1D91;"),
                             conditionalPanel(
                                 condition = "input.tabsetPanel.tabPanel1  == 'General Information'",
                                 sidebarPanel(
                                     selectInput("countries", "Select the country you want to analyse", choices=c("All", unique(as.character(dt.influencers$Audience.Country)))),
                                     p("When selecting a specific country, the data set displayed on the main panel will focus on the influencers that have engagement in that  country")
                                 ),
                                 mainPanel(
                                     p("Reaching the end of our statistical description, you can have a look to the data set used. As we can see, this is presented in 10 lines, divided by 100 different pages. 
                                     Here, we made a selection of the most important variables, namely the name of the account holder that represents the name of the Influencer, the number of followers and the category to which he/she belongs. 
                                     Also, we have the possibility to verify the country where the influencer presents a higher engagement and a column that quantifies this same engagement"),
                                     DT::dataTableOutput('all')
                                     
                                     
                                 )
                             )
                         )
                         
                         
                         
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
                                     
                            ),
                          )
                        )
                      
             )
    )
    
    
    
  )
)

ui
