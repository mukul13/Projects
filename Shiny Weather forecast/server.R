library(ROpenWeatherMap)
library(shiny)

states=c("Andhra Pradesh"
         ,"Arunachal Pradesh"
         ,"Assam"
         ,"Bihar"
         ,"Chhattisgarh"
         ,"Goa"
         ,"Gujarat"
         ,"Haryana"
         ,"Himachal Pradesh"
         ,"Jammu and Kashmir"
         ,"Jharkhand"
         ,"Karnataka"
         ,"Kerala"
         ,"Madhya Pradesh"
         ,"Maharashtra"
         ,"Manipur"
         ,"Meghalaya"
         ,"Mizoram"
         ,"Nagaland"
         ,"Odisha"
         ,"Punjab"
         ,"Rajasthan"
         ,"Sikkim"
         ,"Tamil Nadu"
         ,"Tripura"
         ,"Uttar Pradesh"
         ,"Uttarakhand"
         ,"West Bengal"
         ,"Andaman and Nicobar Islands"
         ,"Chandigarh"
         ,"Nepal"
         ,"Bhutan"
         ,"Bangladesh"
         ,"Dadra and Nagar Haveli","Daman and Diu","National Capital Territory of Delhi" ,"Lakshadweep" ,"Pondicherry") 
len=length(states)

data=NULL
for(i in 1:len)
{
  print(i)
  temp=get_weather_forecast(api_key,city=states[i])
  temp=temp$list$main
  temp$state=states[i]
  temp$index=1:nrow(temp)
  data=as.data.frame(rbind(data,temp))
}
d=gsub("Odisha","Orissa",data$state)
d=gsub("National Capital Territory of Delhi","Delhi",d)
d=gsub("Uttarakhand","Uttaranchal",d)
data$state=d
data$state=as.character(data$state)

shinyServer(function(input, output) {

  output$gvis = renderGvis({
    d1=subset(data,index==input$time)
    #print("processing")
    
 #   gvisGeoChart(d1,
  #               locationvar="state", colorvar="temp",
  #               options=list(region="IN", displayMode="regions", 
   #                           resolution="provinces",
    #                          width=500, height=400,
     #                         colorAxis="{colors:['#FFFFFF', '#0000FF']}"
      #           ))

      G3= gvisGeoChart(d1, 
                      locationvar = "state", 
                      colorvar = input$feature,
                      options=list(region="IN", 
                                   displayMode="regions", 
                                   resolution="provinces",
                                   width=500,height=400))
      Tbl <- gvisTable(d1[,c('state',input$feature)], options=list(height=300), 
                       formats=list(temp="#,###.0"))
      gvisMerge(G3, Tbl, horizontal=T)
  })
  

})
