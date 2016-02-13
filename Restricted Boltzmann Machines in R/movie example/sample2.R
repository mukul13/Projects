library(Rcpp)
library(RcppArmadillo)

### get users data
data=read.table("u.data")
colnames(data)=c("userid","movieid","rating","timestamp")
input=matrix(rep(0,943*1682),nrow = 943,ncol = 1682)

d=matrix(as.numeric(unlist(data),nrow=nrow(data)))
d=matrix(d,nrow=nrow(data))

### to get input matrix
source("movie_data.cpp")
input=get_user_matrix(d,943,1682)

### to train RBM
d=new(rbm,6,10,0.1,6000)
d$train(data)
d1=d$run_visible(test)
d2=d$run_hidden(d1)
print(d2)

### to save weights
d$save_weights()

### to load weights
# d$load_weights();