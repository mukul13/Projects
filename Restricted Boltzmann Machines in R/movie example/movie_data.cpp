// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix get_user_matrix(arma::mat data,long users,long movies)
{
	arma::mat input;
	input.set_size(users,movies);
	input.fill(0);

	int length=data.n_rows;
	for(int i=0;i<length;i++)
	{
		input(data(i,0)-1,data(i,1)-1)=1;
	}

	return wrap(input);
}