library(shiny)
library(REchoNest)
library(tm)

api_key="OHOMFXRMEYVS3MHKW"

#data=NULL

shinyServer(function(input, output) {

output$text1=renderDataTable({
  tryCatch(
    {
  if(input$genre=="")
    data=search_artist(api_key,artist_start_year_after = input$years[1],artist_end_year_before = input$years[2],sort = "hotttnesss-desc",results=50)  
  else
    data=search_artist(api_key,genre = input$genre,artist_start_year_after = input$years[1],artist_end_year_before = input$years[2],sort = "hotttnesss-desc",results=50)
  #  paste("years from ",input$years[1]," to ",input$years[2])
  data$id=NULL
  data
     },
  error=function(cond) {
  
  })
})

output$text2=renderDataTable({
  tryCatch(
    {
  if(input$genre=="")
    data=search_artist(api_key,artist_start_year_after = input$years[1],artist_end_year_before = input$years[2],sort = "hotttnesss-desc")  
  else
    data=search_artist(api_key,genre = input$genre,artist_start_year_after = input$years[1],artist_end_year_before = input$years[2],sort = "hotttnesss-desc")
data$name=as.character(data$name)
tweets=NULL
for(i in 1:nrow(data))
{
  tweets <- tryCatch(
    {
      rbind(tweets,twListToDF(userTimeline(data$name[i],n = 20)))
    },
    error=function(cond) {
      tweets
    }
  )
}

df=cbind(tweets$screenName,tweets$text,tweets$favoriteCount,tweets$retweetCount)
df=as.data.frame(df)
colnames(df)=c("screenName","tweet","favoriteCount","retweetCount")
#index=NULL
df
  },
error=function(cond) {
 
})
  
})

output$artistmap <- renderLeaflet({
  if(input$genre=="")
    data1=search_artist(api_key,artist_start_year_after = input$years[1],artist_end_year_before = input$years[2],sort = "hotttnesss-desc")  
  else
    data1=search_artist(api_key,genre = input$genre,artist_start_year_after = input$years[1],artist_end_year_before = input$years[2],sort = "hotttnesss-desc")
  
  
artist_data=NULL
coords=NULL
  if(!is.null(data1))
  {
    for(i in 1:nrow(data1))
      {
        artist_data=rbind(artist_data,get_artist_data(api_key,data1$name[i],artist_location = T))
      }
    artist_data=as.data.frame(artist_data)
    for(i in 1:nrow(artist_data))
    { 
      coords=rbind(coords,geocode(as.character(artist_data$artist_location[[i]]$location)))    
    }
    coords=as.data.frame(coords)
    coords=cbind(as.character(artist_data$name),coords,as.numeric(artist_data$hotttnesss))
    colnames(coords)=c("name","lon","lat","hotttnesss")
    coords$name=as.character(coords$name)
    
    m= leaflet() %>% addTiles()%>%setView(mean(coords$lon), mean(coords$lat), zoom = 2)%>% 
       addProviderTiles("CartoDB.Positron") %>% 
       addMarkers(coords$lon, coords$lat, popup = paste("<b>Artist: </b>",coords$name,"</br><b>Hotttnesss: </b>",coords$hotttnesss,sep="") )
    m
  }

})

output$text3 = renderDataTable({
  tryCatch(
    { 
      data=search_songs(api_key,artist=input$text21,artist_location = F,results = 100,sort="song_hotttnesss-desc")
      data$id=NULL
      data$artist_id=NULL
      data$song_currency=NULL
      data$artist_familiarity_rank=NULL
      data$artist_discovery_rank=NULL
      data$artist_hotttnesss_rank=NULL
      data
    },
    error=function(cond) {
      
    }
  )
})

output$text4=renderDataTable({
  tryCatch(
    {
  tweets=userTimeline(input$text21,n=70)
  tweets=twListToDF(tweets)
  df=cbind(tweets$screenName,tweets$text,tweets$favoriteCount,tweets$retweetCount)
  df=as.data.frame(df)
  colnames(df)=c("screenName","tweet","favoriteCount","retweetCount")
  df
    },
  error=function(cond)
  {
    
  }
  )
  
},options = list(pageLength=5))

output$text5=renderDataTable({
  tryCatch(
    {
      data=get_top_hottt(api_key,results = 99)
      data$id=NULL
      data
    },
    error=function(cond)
    {
      
    }
  )
  
},options = list(pageLength=5))

output$text6=renderDataTable({
  tryCatch(
    {
      data=similar_artists(api_key,name=input$text21,results = 99)
      data$id=NULL
      data
    },
    error=function(cond)
    {
      
    }
  )
  
},options = list(pageLength=5))

output$artistmap2 <- renderLeaflet({
  tryCatch(
  {
    data=get_artist_data(api_key,name=input$text21,artist_location = T)
  coords=geocode(data$artist_location$location)
  coords=as.data.frame(coords)
  
  m= leaflet() %>% addTiles()%>%setView(coords$lon, coords$lat, zoom = 7)%>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addMarkers(coords$lon, coords$lat, popup = paste("<b>Artist: </b>",input$text21,sep="") )
  m
},
  error=function(cond){
    
  }
  )
})

output$cloud=renderPlot({
  tryCatch(
    {
  data=get_artist_reviews(api_key,name=input$text21,results = 99)
  data=data$summary
  
  ##to make tweets corpus 
  corpus=Corpus(VectorSource(data))
  
  ##to remove punctuation and to make all word lowercase
  corpus=tm_map(corpus,removePunctuation)
  corpus=tm_map(corpus,tolower)
  
  ##to remove stopwords
  corpus=tm_map(corpus,removeWords,c(input$text21,"album",stopwords("english"),"amp","http","http..."))
  corpus=tm_map(corpus,PlainTextDocument)
  
  ## to get frequencies of words
  frequencies=DocumentTermMatrix(corpus)
  #inspect(frequencies[10:12,45:55])
  #findFreqTerms(frequencies,lowfreq=20)
  
  ## to remove terms whose frequency is less than 2%
  corpus=removeSparseTerms(frequencies,0.96)
  corpus=as.matrix(corpus)
  
  fname=colnames(corpus)
  freq=colSums(corpus)
  wordcloud(fname,freq,col=brewer.pal(3,"Set2"),min.freq=10,max.words=35,random.order=F)
},
error=function(cond)
  {
  
}
)
  
})


})


