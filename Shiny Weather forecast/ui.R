
library(googleVis)
library(shiny)

g=list('Temperature'='temp','Minimum Temperature'='temp_min','Maximum temperature'='temp_max','Pressure'='pressure','Sea Level'='sea_level','Ground level'='grnd_level','Humidity'='humidity')

shinyUI(fluidPage(

  # Application title
  titlePanel("Weather forecast using ROpenWeatherMap"),

  
  sidebarLayout(
    sidebarPanel(
      sliderInput("time",
                  h3("Forecast"),
                  min = 1,
                  max = 37,
                  value = 1,animate =  animationOptions(interval = 4000,
                                                        playButton = icon('play', "fa-3x"),
                                                        pauseButton = icon('pause', "fa-3x"))),
      selectInput("feature", label = h3("Feature"), 
                  choices = list(g=g), selected = 1)          
    ),

    
    mainPanel(
    htmlOutput("gvis")
    )
  )
))
