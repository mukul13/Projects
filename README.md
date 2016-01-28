# Projects

### 1. Music data visualization
Shiny App for music data visualization using REchoNest and twitteR

### 2. Shiny Weather Forecast
The above Shiny App is to plot weather forecast data of India. To get weather forecast data, I have written a R package [ROpenWeatherMap](https://github.com/mukul13/ROpenWeatherMap). ROpenWeatherMap is a R wrapper to openweathermap API through which we can access 5 day weather forecast data at any location or city.It includes weather data every 3 hours. I have used googleGvis as a plotting library.
Before running this Shiny App you will have to install ROpenWeatherMap package ([Installation instructions](https://github.com/mukul13/ROpenWeatherMap)). To get OpenWeatherMAP API key, sign up (here)[http://home.openweathermap.org/] 

To run this app:
```R
api_key="YOUR API KEY"
library(shiny)
runApp("/Path to Shiny weather forecast folder")
```
