#setwd("C:/Users/mukul.chaware13/Desktop/N/imdb")

########CLEANING DATA###########

get_all_types=function(mov)
{
  d=paste(mov,collapse=", ")
  d=strsplit(d,", ")
  d=unlist(d)
  len=length(d)
  all=""
  
  for(i in 1:len)
  {
    all=union(all,d[i])
  }
  all[2:length(all)]
}


generate_dataset=function(mov)
{
  genres=get_all_types(mov$Genre)
  mov[genres]=0
  len=nrow(mov)
  
  for(i in 1:len)
  {
    d=strsplit(mov$Genre[i],", ")
    d=unlist(d)
    mov[i,d]=1
  }
  mov  
}

movies=read.csv("movie_updated.csv",stringsAsFactors = F)
movies=generate_dataset(movies)
dups=duplicated(movies$Title)
movies=movies[!dups,]
movies$Metascore=gsub("N/A","NA",movies$Metascore)
movies$Metascore=as.numeric(movies$Metascore)
movies$Title=tolower(movies$Title)
movies$Runtime=as.numeric(gsub(" min","",movies$Runtime))

#########BASIC RECOMMENDER SYSTEM##############
#########ITEM BASED COLLABORATIVE FILTERING###############
#movie="A Beautiful Mind"
item_based_recommendation=function(movies,movie)
{
  movie=tolower(movie)
  col=colnames(movies)[24:length(colnames(movies))]
  col=c("Metascore","imdbRating",col)
  
  features=movies[,col]
  vec1=subset(movies,Title==movie)[1,]
  d=strsplit(vec1$Keywords,", ")
  d=unlist(d)
  
  vec1=vec1[,col]
  vec1[d]=1
  features[d]=0
  
  len=nrow(movies)
  for(i in 1:len)
  {
    f1=strsplit(movies[i,"Keywords"],", ")
    f1=unlist(f1)
    
    common=intersect(d,f1)
    features[i,common]=1
  }
  
  sim<- apply(features,1,function(x) sum(vec1 * x,na.rm=T)/(sqrt(sum(vec1^2,na.rm=T))*sqrt(sum(x^2,na.rm=T))))
  #sim<- apply(features,1,function(x) cor(vec1,x) )
  #sim= sapply(colnames(features), function(metric) {
  #  cor(vec1[metric], features[metric])
  #})
  data=as.data.frame(cbind(movies$Title,sim))
  colnames(data)[1]="Title"
  data=data[order(data$sim,data$Title,decreasing = T),]
  data=subset(data,Title!=movie)
  data
}  


##########USER BASED COLLABORATIVE FILTERING###############

#d=strsplit(movies$Actors,", ")
#d=strsplit(movies$Director,", ")
#d=strsplit(movies$Writer,", ")
#d=strsplit(movies$Country,", ")

get_movies_by_common_actors=function(vec1,data)
{
  f1=strsplit(vec1$Cast,", ")
  f2=strsplit(data$Cast,", ")
  len=nrow(data)  
  ans=array(F,len)
  #d=apply(f2,1,function(x) sum(f1[[1]]==x[[1]]))
  len2=length(f1[[1]])
  for(i in 1:len)
  {
    for(j in 1:len2)
    {
      if(!is.na(match(f1[[1]][j],f2[[i]])) )
    {
      ans[i]=T
    }
      }
  }
  d=subset(data,ans==T)
d
}

get_movies_by_common_directors=function(vec1,data)
{
  f1=strsplit(vec1$Director,", ")
  f2=strsplit(data$Director,", ")
  len=nrow(data)  
  ans=array(F,len)
  #d=apply(f2,1,function(x) sum(f1[[1]]==x[[1]]))
  len2=length(f1[[1]])
  for(i in 1:len)
  {
    for(j in 1:len2)
    {
      if(!is.na(match(f1[[1]][j],f2[[i]])) )
      {
        ans[i]=T
      }
    }
  }
  d=subset(data,ans==T)
  d
}

get_movies_by_common_writers=function(vec1,data)
{
  f1=strsplit(vec1$Writer,", ")
  f2=strsplit(data$Writer,", ")
  len=nrow(data)  
  ans=array(F,len)
  #d=apply(f2,1,function(x) sum(f1[[1]]==x[[1]]))
  len2=length(f1[[1]])
  for(i in 1:len)
  {
    for(j in 1:len2)
    {
      if(!is.na(match(f1[[1]][j],f2[[i]])) )
      {
        ans[i]=T
      }
    }
  }
  d=subset(data,ans==T)
  d
}

get_movies_by_common_genres=function(vec1,data)
{
  f1=strsplit(vec1$Genre,", ")
  f2=strsplit(data$Genre,", ")
  len=nrow(data)  
  ans=array(F,len)
  #d=apply(f2,1,function(x) sum(f1[[1]]==x[[1]]))
  len2=length(f1[[1]])
  for(i in 1:len)
  {
    for(j in 1:len2)
    {
      if(!is.na(match(f1[[1]][j],f2[[i]])) )
      {
        ans[i]=T
      }
    }
  }
  d=subset(data,ans==T)
  d
}

get_movies_by_common_production=function(vec1,data)
{
  f1=strsplit(vec1$Production,", ")
  f2=strsplit(data$Production,", ")
  len=nrow(data)  
  ans=array(F,len)
  #d=apply(f2,1,function(x) sum(f1[[1]]==x[[1]]))
  len2=length(f1[[1]])
  for(i in 1:len)
  {
    for(j in 1:len2)
    {
      if(!is.na(match(f1[[1]][j],f2[[i]])) )
      {
        ans[i]=T
      }
    }
  }
  d=subset(data,ans==T)
  d
}

recommendation=function(movies,movie)
{
  movie=tolower(movie)
  vec1=subset(movies,Title==movie)[1,]
  d1=as.data.frame(get_movies_by_common_directors(vec1,movies)) 
  d1=rbind(d1,as.data.frame(get_movies_by_common_actors(vec1,movies)))
  d1=rbind(d1,as.data.frame(get_movies_by_common_writers(vec1,movies)))
  d1=rbind(d1,as.data.frame(get_movies_by_common_production(vec1,movies)))
  
  col=colnames(movies)[24:length(colnames(movies))]
  col=c("Metascore","imdbRating",col)
  
  features=movies[,col]
  d=strsplit(vec1$Keywords,", ")
  d=unlist(d)
  
  vec1=vec1[,col]
  vec1[d]=1
  features[d]=0
  
  len=nrow(movies)
  for(i in 1:len)
  {
    f1=strsplit(movies[i,"Keywords"],", ")
    f1=unlist(f1)
    
    common=intersect(d,f1)
    features[i,common]=1
  }
  
 sim<- apply(features,1,function(x) sum(vec1 * x,na.rm=T)/(sqrt(sum(vec1^2,na.rm=T))*sqrt(sum(x^2,na.rm=T))))
 data=as.data.frame(cbind(movies$Title,sim))
 colnames(data)[1]="Title"
 data=data[order(data$sim,data$Title,decreasing = T),]
 data=subset(data,Title!=movie)
 data
}