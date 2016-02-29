###importing libraries
library(XML)
library(omdbapi)
library(RCurl)
library(httr)
library(rvest)
library(tm)
############################################

###Proxy settings

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

###get imdb ids
getImdbID=function(movie)
{
  omdb=search_by_title(movie)
  omdb$imdbID[1]
}

################################
scrapeReviews=function(movie)
{
  id=getImdbID(movie)
  
  print(1)
  rev=read_html(getURL(paste("http://www.imdb.com/title/",id,"/reviews?ref_=tt_urv")),sep="")
  
  reviews=rev %>% html_nodes("#tn15content div+ p") %>% html_text()
  
  rev_title=rev %>% html_nodes("h2") %>% html_text() 
  
  author=rev %>% html_nodes("#tn15content div a") %>% html_text() 
  author=subset(author,author!="")
  
  date=rev %>% html_nodes("br+ small") %>% html_text()
  
  quality=rev %>% html_nodes("#tn15content small:nth-child(1)") %>% html_text()
  quality=sub(" people found the following review useful:","",quality)
 
  #for location a+ small 
  count=10
  
for(i in 2:800)
{
  print(i)
  rev=read_html(getURL(paste("http://www.imdb.com/title/",id,"/reviews?start=",count)),sep="")
  temp=rev %>% html_nodes("#tn15content div+ p") %>% html_text()
  
  if(length(temp)==0)
    break
  
  reviews=c(reviews,temp)
  rev_title=c(rev_title,rev %>% html_nodes("h2") %>% html_text())
  
  temp=rev %>% html_nodes("#tn15content div a") %>% html_text() 
  temp=subset(temp,temp!="")
  
  author=c(author,temp)
  
  date=c(date,rev %>% html_nodes("br+ small") %>% html_text())
  
  temp=rev %>% html_nodes("#tn15content small:nth-child(1)") %>% html_text()
  temp=sub(" people found the following review useful:","",temp)
  quality=c(quality,temp)
  count=count+10
}
data=as.data.frame(cbind(author,date,rev_title,reviews,quality))
data  
}

get_reviews=function(movie)
{
  data=scrapeReviews(movie)
  write.csv(data,file=paste(movie,"_reviews.csv",sep=""),row.names = F)
}

#################ANALYSIS######################

bag_of_words=function()
{
#setwd("C:/Users/mukul.chaware13/Desktop/N/imdb")

#data=read.csv(paste(movie,"_reviews.csv",sep=""),stringsAsFactors = F)
data=read.csv("The dark knight_reviews.csv",stringsAsFactors = F)

d=strsplit(data$date," ")
len=nrow(data)
for(i in 1:len)
{
  data$day[i]=d[[i]][1]
  data$month[i]=d[[i]][2]
  data$year[i]=d[[i]][3]
}

data$day=as.numeric(data$day)
data$month=as.factor(data$month)
data$year=as.numeric(data$year)

corpus=Corpus(VectorSource(data$reviews))

##to remove punctuation and to make all word lowercase
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,tolower)

##to remove stopwords
corpus=tm_map(corpus,removeWords,c(stopwords("english"),"amp","http","movies","film","movie","http...","though","just","also","well","yet","still","much","can","every"))
corpus=tm_map(corpus,PlainTextDocument)

## to get frequencies of words
frequencies=DocumentTermMatrix(corpus)
#inspect(frequencies[100:102,45:55])
#findFreqTerms(frequencies,lowfreq=20)

## to remove terms whose frequency is less than 2%
corpus=removeSparseTerms(frequencies,0.995)
corpus=as.matrix(corpus)

fname=colnames(corpus)
freq=colSums(corpus)
wordcloud(fname,freq,col=brewer.pal(3,"Set2"),min.freq=15,max.words=50,random.color = T,random.order=F)
}