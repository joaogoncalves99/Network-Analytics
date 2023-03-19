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
        fluidRow(
          column(width = 10,   
            h3("Welcome to our Shiny App!",style = "margin-top: 10x; 
                    background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                    -webkit-background-clip: text;
                    -webkit-text-fill-color: transparent;
                    font-weight: bold;
                    font-size: 28px;
                    letter-spacing: -1px;"
              ),
            tags$hr(),
            p("This project is focused on exploring and analyzing a network of 1000 Instagram influencers. The dataset includes valuable information such as the influencers' audience country of origin, number of followers, and category of their profile.
              Our goal with this project is to identify the most influential individuals in this network and gain insights into their connections and impact."),
            p("In the \"Descriptive Statistics\" tab, you can view aggregated data about the influencers and their followers. This includes general statistics about the number of followers and their country of origin, as well as more specific data such as the most popular categories of influencer profiles."),
            p("In the \"Network Exploration\" tab, you can view various descriptive statistics regarding the network, such as the number of top influencers, as well as visual representations of the connections between influencers."),
            p("Finally, in the \"Link Prediction\" tab, we have created a predition tool that predicts which influencers are similar to each other and plotted the connections. We also calculated Jaccard Index to helped us to see the predictions better."),
            p("Overall, our Top Influencers project provides a comprehensive analysis of the network of 1000 influencers and their network. Maybe in the future, this tool will help you make informed decisions about your influencer marketing strategy and improve your overall marketing efforts."),
            tags$h1("Our Team",
                    style = "margin: 0; 
             background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
             -webkit-background-clip: text;
             -webkit-text-fill-color: transparent;
             font-weight: bold;
             font-size: 28px;
             letter-spacing: -1px;"),
          ),
        ),
        tableOutput("my_table"),
        column(width = 3, 
                tags$img(src = "kand.png", height = 200)
        ),
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
            h3("Summary Statistics", style = "margin-top: 10x; 
                    background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                    -webkit-background-clip: text;
                    -webkit-text-fill-color: transparent;
                    font-weight: bold;
                    font-size: 28px;
                    letter-spacing: -1px;"),
            p("In this page you can find the key summary statistics on the collected data. On the sidebar you can select the range 
              of Influencers you want to look at and select the summary statistic you want to display and analyze."),
            p("The 'Followers Distibution' histogram shows the statistical distribution for the variable 'Followers' in our dataset."),
            p("In the 'Followers Distibution by Country' graph you can see the number of Influencers with a certain number of followers 
              by each country."),
            p("In the 'Followers Distribution by Category' graph you can see the number of Influencers with a certain number of followers
              by each category in our dataset."),
            p("The 'Authentic Engagement Distribution' histogram shows the statistical distribution for the variable 'Authentic Engagement'
              in our dataset."),
            p("In the 'Followers Distribution by Category' graph you can see the authentic engagement for every influencer that is present
              in each country."),
            p("In the 'Followers Distribution by Category' graph you can see the authentic engagement for every influencer that belongs to 
              each category."),
            tags$hr(),
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
                selectInput("statistics", "Select the statistics you want to look at", 
                            choices=c("Followers Distribution" , "Followers Distribution by Country", 
                                      "Followers Distribution by Category",  "Engagement Distribution", 
                                      "Engagement Distribution by Country", "Engagement Distribution by Category"))
               ),
              mainPanel(
                plotOutput("plot"),
              ),
            )
          ),
          tabPanel("All about Influencers",
                   h2("Influencer Summary Statistics",style = "margin-top: 10px; 
                background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                font-weight: bold;
                font-size: 28px;
                letter-spacing: -1px;"),
                p("In this tab you can find an overview of the influencers from our dataset."),
                p("Here you can search for the title of every influencer's account in our dataset, the number of followers of each influencer,
                  the categories to which each influencer belongs to and the global authentic engagement of each account (number of likes and 
                  comments that, on average, come from 'real people' and influencers per post)."),
                p("The 'rank' variable accounts to a popularity rank that we have created, in which all the influencers in the dataset are
                  grouped in 5 different categories from 1 to 5, according to their popularity (number of followers), being 5 the highest level 
                  of popularity."),
                p("You can also plug in the name of an influencer in the 'Search' tab if you want to analyze a specific one."),
                tags$hr(),
              fluidRow(
                dataTableOutput("influencers.metrics")
                )    
          ),
          tabPanel("All about Categories",
                   h2("Categories Summary Statistics",style = "margin-top: 10px; 
                background: -webkit-linear-gradient(#8a3ab9, #fd1d1d);
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                font-weight: bold;
                font-size: 28px;
                letter-spacing: -1px;"),
                   p("In this tab you can find an overview of the main statistics for the Categories in our dataset."),
                   p("Here you can choose a country that you want to analyze and check the metrics table for each category 
                     that is present in the country that you have chosen, as well as the world cloud with all the categories present in that 
                     country."),
                   p("In the 'Categories Metrics Table', you can find the 'Frequency' column, which accounts for the total number of influencers
                     that belong to that category, the average authentic engagement of each category, the minimum and the maximum authentic 
                     engagement of each category and also the standard deviation of each one, all of this for the country you have chosen."),
                   p("You can also plug in the name of a category in the 'Search' tab if you want to analyze a specific one."),
                   p("In the 'Country Categories Wordcloud' you can see all the categories present in the country you have chosen, being the bigger
                     words the most popular influencer categories in that country."),
                   tags$hr(),
                   selectInput("country.cat", "Choose a country:",
                               choices = c(unique(dt.influencers.new$Audience_country)),
                               selected = "United States",
                               multiple = F),
                  
                   tabsetPanel(
                     tabPanel("Categories Metrics Table",
                              dataTableOutput("categories.metrics")),
                     
                     tabPanel("Country Categories Wordcloud",
                     wordcloud2Output("wordcloud.metrics"))
        
                   )
                  
                  
          ),
        
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
              p("Here we find an interactive network model that highlights which influencers are connected, based on the category of their 
                content. The user can interact with the network model by clicking in each node in order to get a better insight on the existing
                connections of each influencer."),
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
              p("Here we find an interactive network model that highlights which influencers are connected, based on their audience country. 
                The user can interact with the network model by clicking each node in order to get a better insight on the existing connections 
                of each influencer."),
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
               p("With all the available data, we can predict link formation in the influencers network. For doing that, we will be using 
                 the Common Neighbors measure of similarity. Since one influencer can have more than one category, we will be predicting 
                 if, in the future, there will be more connections between influencers based on the new categories their Instagram profile
                 might have. In the graph, we can see those future connections."),
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
                     p("Here, we will use the Jaccard Index to determine which edges between pairs of influencers are more likely to appear
                       in the future. The Jaccard Index consists in a number between 0 and 1, which can be seen in the similarity column in
                       the table below. The user can sort it so that he can find the needed information."),
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
