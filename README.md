# Projects
 * Music data visualization
 * Shiny Weather Forecast
 * Basic neural network implementation using Rcpp and RcppArmadillp
 * Restricted Boltzmann Machines in R


### 1. Music data visualization
Shiny App for music data visualization using REchoNest and twitteR

##### TODO
* to make ui user friendly.(current shiny ui is very messy.)

### 2. Shiny Weather Forecast
The above Shiny App is to plot weather forecast data of India. To get weather forecast data, I have written a R package [ROpenWeatherMap](https://github.com/mukul13/ROpenWeatherMap). ROpenWeatherMap is a R wrapper to openweathermap API through which we can access 5 day weather forecast data at any location or city.It includes weather data every 3 hours. I have used googleGvis as a plotting library.
Before running this Shiny App you will have to install ROpenWeatherMap package ([Installation instructions](https://github.com/mukul13/ROpenWeatherMap)). To get OpenWeatherMAP API key, sign up (here)[http://home.openweathermap.org/] 

To run this app:
```R
api_key="YOUR API KEY"
library(shiny)
runApp("/Path to Shiny weather forecast folder")
```

![sample app image](https://github.com/mukul13/Projects/blob/master/Shiny%20Weather%20forecast/sample%20Image.JPG)

### 3. Basic neural network implementation using Rcpp and RcppArmadillp
The 'Rcpp' package provides R functions as well as C++ classes which offer a seamless integration of R and C++.RcppArmadillo provides an interface from R to and from Armadillo by utilising the Rcpp R/C++ interface library.

```R
library(Rcpp)
library(RcppArmadillo)
source(neural_network_2.cpp)

### to call constructor of neural_network class
### new(neural_network,num_input_neurons,num_hidden_neurons,num_output_neurons)
nn=new(neural_network,10,64,1)

### to set learning parameters
### nn$set_learning_parameters(learning_rate,momentum)
nn$set_learning_parameters(0.01,0.01)

### to set maximum number of epochs
### nn$set_max_epochs(max_epochs)
nn$set_max_epochs(2000)
````

To train neural network

```R
### to train neural network, first convert input data into list as follows
### train=matrix(as.numeric(unlist(data2)),nrow=nrow(data2))
### train=list(data=training_part,label=matrix(labels))
### sample code is given in sample.R file

### nn$train_network(train_data_list,gen_data_list,val_data_list)
nn$train_network(train,gen,val)
```

To save or load weights

```R
### to save weights
nn$save_weights()

### to load weights
nn$load_weights()
```

To predict on test datasets

```R
### first convert test data into matrix
### test=matrix(as.numeric(unlist(test_data)),nrow=nrow(test_data))

### to predict on test data matrix
pred=nn$predict(test)
```
##### Resources
[Neural network introduction](https://takinginitiative.wordpress.com/2008/04/03/basic-neural-network-tutorial-theory/)

##### TODO
* to optimize C++ code (by removing for loops and using RcppArmadillo's builtin functions for matrix multiplication)

### 4. Restricted Boltzmann Machines in R
Sample codes are given in sample.R and sample2.R

```R
### to load RBM code
library(Rcpp)
library(RcppArmadillo)
source("rbm.cpp")

### to initialize RBM object
new(rbm,num_visible,num_hidden,learning_rate,max_epochs)

### to train RBM
d$train(train_matrix)

### to predict
d1=d$run_visible(test_matrix)
d$run_hidden(d1)

### to save weights
d$save_weights()

### to load weights
d$load_weights()
```

##### Resources
[RBM Introduction](https://github.com/echen/restricted-boltzmann-machines)

##### TODO
* add daydream function
