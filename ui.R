library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(visNetwork)

css <- '.nav-tabs>li>a {
  font-family: "Lucida Sans", sans-serif;
  color: #FD1D91;
}'

# Define UI
ui <- fluidPage(
  tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "image.png", 
      height = "40",
      width = "40px",
      style = "margin-right: 7px; margin-left: 15px; margin-bottom:5px; margin-top:5px"
    ),
    tags$h1("Instagram Shiny App",
            style = "margin: 0; 
             background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
             -webkit-background-clip: text;
             -webkit-text-fill-color: transparent;
             font-weight: bold;
             font-size: 28px;
             letter-spacing: -1px;"),
  ),
  tags$head(tags$style(HTML(
    '.nav-tabs>li>a {
    font-family: "Lucida Sans", sans-serif;
    color: #FD1D91;
    }'
  ))),
  mainPanel(
    tabsetPanel(
      id = "mainTabset",
      tabPanel("Home",
    
       h1("Hi!"),
       h2("Welcome to our Shiny app! We hope you enjoy it as much as we enjoyed creating it."),
       h3("The dataset contains information on the 1,000 most popular influencers,  including their number of followers, engagements")
       
      ),
      
      tabPanel("Descriptive Statistics",
      #Code for descriptive statistics
        tabsetPanel(
          id = "descriptiveTabset",
          tabPanel("General Data",
                   h2("General Descriptive Statistics", style = "margin-top: 10x; 
                    background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                    -webkit-background-clip: text;
                    -webkit-text-fill-color: transparent;
                    font-weight: bold;
                    font-size: 28px;
                    letter-spacing: -1px;"),
                   p("This page will provide the most important statistic metrics on the collected data."),
                   tags$hr(),
            column(5, selectInput("rank", "Rank:", choice= c(1,2,3,4,5), selected = 1, multiple = F),
                   tableOutput("general.statistics")
                   ),
            column(7,textOutput("general.text"))
          ),
          
          tabPanel("Summary Statistics",
            sidebarPanel(
              sliderInput("bins",
                          "Number of bins:",
                          min = 1,
                          max = 100,
                          value = 20),
              
              radioButtons("tops", "Choose the Top Influencer you want to look at", 
                           choices=c("Top 10", "All"),
                           selected="Top 10"),
              
              selectInput("countries",
                          label= "Select which country you want to analyse", 
                          choices="ola", 
                          selected="United States of America",
                          multiple=F)
            ),
                    #ADD MAIN PANEL HERE
          ),
          tabPanel("All about your Influencer"
                   #ADD MAIN PANEL HERE
          ),
          tabPanel("Get more Specific"
                   #ADD MAIN PANEL HERE
          )
        )
      ),
      
      tabPanel("Network Exploration",
       sidebarPanel(
         h5("Here you can change the parameters of the network you would like to see."),
         
         textInput("top.followers.input", "Top Influencers to Display (based on followers):",value = 10),
         
         sliderInput("nfollowers.input", "Minimum Number of Followers (millions):",
                     min = 0, max = 470, value = 0, step = 25),
         
         checkboxInput("no.connection.input", "Delete nodes which have no connection to other nodes", value = FALSE),
       ),
       
       mainPanel(
         tabsetPanel(
           id = "dataTabset",
            tabPanel("Overall Statistics",
              h2("Summary Statistics and Centrality Analysis",style = "margin-top: 10px; 
                background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                font-weight: bold;
                font-size: 28px;
                letter-spacing: -1px;"),
              p("Here we can find both some basic descriptive statistics on the network and information on the centrality measures."),
                selectInput("two.choices", "Network of influencers based on:",
                          choices = c("Category", "Country"),
                          selected = "Category"),
              tags$hr(),
              
              fluidRow(
                tableOutput("network.statistics")),
                    
              h3("Degree distribution of the influencers' network"),
                    
              fluidRow(
                column(9,plotOutput("histogram.statistics")),
                column(3,sliderInput("bins.input", "Number of Bins",
                       min = 0, max = 10, value = 5, step = 5))),
                    
              h3("Centrality measures for each influencer"),
                    
              fluidRow(
                dataTableOutput("network.metrics")
              )
            ),
            tabPanel("Influencers Based on Category",
              h2("Network of Influencers, based on Category",style = "margin-top: 10px; 
                background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                font-weight: bold;
                font-size: 28px;
                letter-spacing: -1px;"),
              p("Here we find an interactive network model that highlights which influencers are connected, based on the category of their content. The user can interact with the network model by clicking in each node in order to get a better insight on the existing connections of each influencer."),
              selectInput("category.choices", "Choose one or more categories:",
                           choices = c(unique(dt.influencers.new$Category)),
                           selected = "Fashion",
                           multiple = T),
              tags$hr(),
              visNetworkOutput("category.network",height = "800px", width = "1000px")
            ),
            tabPanel("Influencers Based on Audience Country",
              h2("Network of Influencers, based on Audience Country",style = "margin-top: 10px; 
                background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                font-weight: bold;
                font-size: 28px;
                letter-spacing: -1px;"),
              p("Here we find an interactive network model that highlights which influencers are connected, based on their audience country. The user can interact with the network model by clicking each node in order to get a better insight on the existing connections of each influencer."),
              selectInput("countries.choices", "Choose one or more countries:",
                          choices = c(unique(dt.influencers.new$Audience_country)),
                          selected = "India",
                          multiple = T),
              tags$hr(),
              visNetworkOutput("country.network",width = "1000px", height = "800px")
            )
          )
        )
      ),
      tabPanel("Link Prediction",
        mainPanel(
          tabsetPanel(
            tabPanel("Common Neighbors",
               h2("Common Neighbors",style = "margin-top: 10px; 
          background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          font-weight: bold;
          font-size: 28px;
          letter-spacing: -1px;"),
               p("adicionar"),
               textInput("top.influencers", "Top Influencers to Display (based on followers):",value = 10),
               tags$hr(),
               visNetworkOutput("prediction.network",width = "1000px", height = "800px")
            ),
            tabPanel("Jaccard Index",
                     h2("Jaccard Index",style = "margin-top: 10px; 
                      background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                      -webkit-background-clip: text;
                      -webkit-text-fill-color: transparent;
                      font-weight: bold;
                      font-size: 28px;
                      letter-spacing: -1px;"),
                     p("adicionar"),
                     textInput("top.influencers.jaccard", "Top Influencers to Display (based on followers):",value = 10),
                     sliderInput("interval.jaccard", label = "Select an Interval:",
                                 min = 0, max = 1, value = c(0, 0.5), step = 0.1),
                     tags$hr(),
                     dataTableOutput("jaccardi.index")  
            ),
          )
        )
      )
    )
  )
)
ui
