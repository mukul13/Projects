library(shiny)
library(REchoNest)
library(leaflet)
library(ggmap)
library(twitteR)
library(wordcloud)

requestURL="https://api.twitter.csom/oauth/request_token"
accessURL="https://api.twitter.com/oauth/access_token"
authURL="https://api.twitter.com/oauth/authorize"
consumerKey="jOP91eLqcrlxprYCMgH9AeWd4"
consumerSecret="DSGBixATCwwVZy7RoUuDjqh7z82vMFfhWlJ4zV5d5sbCbnHlk7"
accessToken="3369300031-RUeCuOUSZoIgCuoxzxOxWXf9ik608RKbnGZCVsL"
accessSecret="acwuhSqWanZlOrNGBNmu1WEKnHS3pRBGN3U4XyaXcvdJZ"

## to make connections
options(httr_oauth_cache=T)
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessSecret)
print("Authentication done")

api_key="OHOMFXRMEYVS3MHKW"
g=read.csv("C:/Users/mukul.chaware13/Desktop/N/Music API/Music data visualization/data/genre.csv",stringsAsFactors = F)
g=g$x

shinyUI(navbarPage("REchoNest!",
tabPanel("Genre",  
  sidebarLayout(
    sidebarPanel( h2("Input"),
                  sliderInput("years","Years",min=1960,max=2016,value = c(1975,2004)),
                                 selectInput("genre", label = h3("Genres"), 
                              choices = list(g=g), selected = 1)
                  
                ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Suggested Artists", 
                   h3("Artists"),
                  dataTableOutput("text1")
                          ),
        tabPanel("Tweets",h3("Tweets by Artists"),
                 dataTableOutput("text2")),
        tabPanel("Map",leafletOutput("artistmap",width="600px",height="400px")) 
        )
    )
              )
  ),

tabPanel("Artist",  
         sidebarLayout(
           sidebarPanel(
             textInput("text21", label = h3("Artist Name"), 
                       value = "")
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("Artist Info",
                        h4("Tweets by Artist"),
                        dataTableOutput("text4"),
                        br(),
                        br(),
                        h4("Recommended Artists"),
                        dataTableOutput("text6"),
                        br(),
                        br(),
                        h4("Artist location"),
                        leafletOutput("artistmap2",width="200px",height="200px"),
                        br(),
                        br(),
                        h4("Top Hottt Artist"),
                        dataTableOutput("text5"),
                        br(),
                        br()
                        ),
               tabPanel("Songs", h3("Artsist's Songs"),
                        dataTableOutput("text3")),
               tabPanel("Wordcloud",plotOutput("cloud"))
             
             )
                 )
           )
           
         )
)

)