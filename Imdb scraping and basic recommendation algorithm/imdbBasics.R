library(httr)
library(RCurl)
library(XML)
library(omdbapi)
library(rvest)

### proxy setting
#opts <- list(
#  proxy         = "*********",
#  proxyusername = "****", 
#  proxypassword = "****", 
#  proxyport     = ****
#)

#set_config(
#  use_proxy(url="*********", port=****, username="****",password="******")
#)

#options(RCurlOptions = opts)

#movie=htmlParse(getURL("http://www.imdb.com/title/tt0816692/reviews?ref_=tt_urv"))
#browseURL("http://www.imdb.com/title/tt0816692/")

get_movie_details=function(movie)
{
m=search_by_title(movie)
m=m[1,]
id=m$imdbID

  if(!is.null(id))
  {
  data=find_by_id(id)
  data=as.data.frame(data)
  
  keywords= tryCatch({
    key=read_html(getURL(paste("http://www.imdb.com/title/",id,"/keywords?ref_=tt_stry_kw")),sep="")
    key %>% html_nodes(".sodatext a") %>% html_text()
    
              },
              error=function(cond)
              {
                NA
                
              })
  
  cast=tryCatch({
    castHTML=read_html(getURL(paste("http://www.imdb.com/title/",id,"/")),sep="")  
    castHTML %>% html_nodes(".itemprop .itemprop") %>% html_text()
  },
                error=function(cond)
                  {
                  NA
                })
  
  
  prod=tryCatch({
    prodHTML=read_html(getURL(paste("http://www.imdb.com/title/",id,"/companycredits?ref_=tt_dt_co")),sep="")  
    prodHTML%>% html_nodes("#production+ .simpleList a") %>% html_text()
    
  },error=function(cond)
    {
    NA
  })
  
  keywords=paste(keywords,collapse=", ")
  data$Keywords=keywords[1]
  cast=paste(cast,collapse=", ")
  data$Cast=cast[1]
  prod=paste(prod,collapse=", ")
  data$Production=prod[1]
  
data=as.data.frame(data)
data
}
}


get_movie_data=function(movies)
{
  len=length(movies)
  print(1)
  data=get_movie_details(movies[1])
  
d=tryCatch({
  
  for(i in 2:len)
  {
    print(i)
    data=rbind(data,get_movie_details(movies[i]))
  }
  data
},error={
  data
})
d
}

get_all_data=function(movies)
{
  len=length(movies)
  print(1) 
  m=search_by_title(movies[1])

    data=get_movie_data(m$imdbID)
 
    if(len!=1)
  {
      for(i in 2:len)
  {
    print(i)
    m=search_by_title(movies[i])
    data=rbind(data,get_movie_data(m$imdbID))
  }
  }
  data
}

csv_data=function()
{
  #setwd("C:/Users/mukul.chaware13/Desktop/N/imdb")
  movies=read.csv("movie.csv",header=F,stringsAsFactors = F)
  movies=movies$V1
final_data=get_movie_data(movies[4801:length(movies)])
#final_data=get_all_data(movies[1:5])
final_data
}
#final_data=csv_data()